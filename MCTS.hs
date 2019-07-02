{-# LANGUAGE ExistentialQuantification, BangPatterns, NamedFieldPuns, RankNTypes #-}

module MCTS where

import SolverDefs
import Data.Maybe
import System.Random
import Data.Time
import Data.List
import Data.Ord
import Control.Concurrent.Async
import Control.Concurrent.MVar
import System.IO
import qualified PQueueQuick as PQ
import qualified Data.Map as M

-- | exploration/expolitation are coefficients for the UCB funtion
--   alpha, beta are values that will stop the search
--   duration is the amount of time in milliseconds to search
--   maxsim is the maximum count of simulations
--   background is True when we allow thinking in the background
--   uniform is True when we want to explore and not to exploit only for the first node
--   numrolls is the number of rollouts per leaf
--   simsperroll is the rate of increase of the rollouts per leaf per root simulations
--   inert is when we do not want to propagate terminals
data MCParams = MCParams
  {exploitation :: {-# UNPACK #-} !Value, exploration :: {-# UNPACK #-} !Value,
   alpha :: {-# UNPACK #-} !Value, beta :: {-# UNPACK #-} !Value,
   duration ::  {-# UNPACK #-} !Int, maxsim :: {-# UNPACK #-} !Int,
   numrollsI :: {-# UNPACK #-} !Int, numrollsSqrt :: {-# UNPACK #-} !Value,
   simsperroll :: {-# UNPACK #-} !Value,
   background :: !Bool, uniform :: !Bool, inert :: !Bool}

data MCSolvedGame = MCSolvedGame {mcNode :: MCNode, mcParams :: MCParams}

data MCNode = forall gs. (GameState gs) =>
    MCNode {gameState :: !gs,
            simulations :: {-# UNPACK #-} !Value,
            wins :: {-# UNPACK #-} !Value,
            children :: MCActions
            }

type MCAction = (String, MCNode)

data PrioAction = PrioAction {priority :: {-# UNPACK #-} !Value,
                              subsims :: {-# UNPACK #-} !Value,
                              action' :: MCAction}

instance Eq PrioAction where
  mc1 == mc2 = (fst $! action' $! mc2) == (fst $! action' $! mc1)

instance Ord PrioAction where
  compare mc1 mc2 = compare (priority $! mc2) (priority $! mc1)

-- | Terminals are states with a known value
--   Trunks are states with fully evaluated children
--   Buds are states where some children have not yet been evaluated
data MCActions = Terminal Value [MCAction]
               | forall gs. GameState gs => Bud [MCAction] [(String, gs)]
               | Trunk (PQ.PQueue PrioAction) [MCAction]
               | InertTerminal Value

-- | Defines a preference over nodes
--   The first argument is zero for terminals, otherwise it's wins/simulations
data NodeValue = NodeValue Value Value deriving Eq

-- | Combine preferences of the same nodes in multiple instances
combineValues :: NodeValue -> NodeValue -> NodeValue
combineValues (NodeValue a 0) _ = NodeValue a 0
combineValues _ (NodeValue x 0) = NodeValue x 0
combineValues (NodeValue a b) (NodeValue x y) = NodeValue (a+x) (b+y)

instance Ord NodeValue where
  compare (NodeValue a b) (NodeValue x y) = compare (a/(b+1)) (x/(y+1))

-- | Gets the node's value
nodeValue :: MCNode -> NodeValue
nodeValue (MCNode {children = Terminal !val _}) = NodeValue val 0
nodeValue (MCNode {wins, simulations}) = NodeValue wins simulations

defaultMCParams :: MCParams
defaultMCParams = MCParams {exploitation = 1, exploration = sqrt 2,
                            alpha = (-1), beta = 1,
                            duration = 1000, maxsim = 100000000, background = True,
                            numrollsI = 1, numrollsSqrt = 1, simsperroll = 1000000,
                            uniform = False, inert = False}

playerBound :: Player -> MCParams -> Value
playerBound Maximizer = beta
playerBound Minimizer = alpha

playerObjectiveBy :: (Foldable t) => Player -> (a -> a -> Ordering) -> t a -> a
playerObjectiveBy Maximizer = maximumBy
playerObjectiveBy Minimizer = minimumBy

-- | Getter function from a terminal
terminalVal :: MCNode -> Maybe Value
terminalVal !(MCNode {children = !(Terminal !v _)}) = Just v
terminalVal _ = Nothing

-- | Average value for the node
avgVal :: MCNode -> Value
avgVal mcNode = (wins $! mcNode) / (simulations $! mcNode)

instance Show MCSolvedGame where
  show MCSolvedGame {mcNode = MCNode {gameState}} = show gameState

instance GameState MCSolvedGame where
  player MCSolvedGame {mcNode = MCNode {gameState}} = player gameState
  terminal MCSolvedGame {mcNode = MCNode {gameState}} = terminal gameState
  maxdepth MCSolvedGame {mcNode = MCNode {gameState}} = maxdepth gameState
  actions MCSolvedGame {mcParams, mcNode = MCNode {children}} =
    map mkSolvedGame $ mkActions children where
      mkSolvedGame (str, mcNode) = (str, MCSolvedGame {mcParams, mcNode})

instance SolvedGameState MCSolvedGame where
  action (MCSolvedGame {mcParams, mcNode = node@(MCNode {gameState})}) = do
    newnode <- timedadvance mcParams node
    toMCSolvedGame <$> best newnode
    where
      best (MCNode {children = Terminal v terminals}) = do
        let actions' = filter ((== Just v) . terminalVal . snd) terminals
        if v == playerBound player' mcParams
          then return $ head actions'
          else do
            str <- lessevilMCTS mcParams gameState $ map fst actions'
            return (str, fromJust $ lookup str actions')
      best (MCNode {children = Trunk nonterminals []}) = return $
        objective (comparing $! nodeValue . snd) $ map action' $ PQ.toAscList nonterminals
      best (MCNode {children = Trunk nonterminals terminals}) = do
        let bestnt = objective (comparing $ nodeValue . snd) $ map action' $ PQ.toAscList nonterminals
            bestter = objective (comparing $ nodeValue . snd) terminals
            ternv@(NodeValue v _) = nodeValue $ snd bestter
            takent = case player' of
              Maximizer -> ternv < (nodeValue $ snd bestnt)
              Minimizer -> ternv > (nodeValue $ snd bestnt)
        if takent then return bestnt
          else do
            let actions' = filter ((== Just v) . terminalVal . snd) terminals
            str <- lessevilMCTS mcParams gameState $ map fst actions'
            return (str, fromJust $ lookup str actions')
      objective = playerObjectiveBy $! player'
      player' = player gameState
      toMCSolvedGame (str, mcNode) = (str, MCSolvedGame {mcParams, mcNode})
  think (MCSolvedGame {mcParams, mcNode}) = do
    newnode <- advanceuntil mcParams mcNode
    return $ (\mcNode -> MCSolvedGame {mcParams, mcNode}) <$> newnode

-- | Perform rollouts and update the node
rolloutNode :: RandomGen rg => Int -> MCNode -> rg -> (MCNode, rg)
rolloutNode !n !mcNode@(MCNode {gameState = gs}) !rand = (mcNode', rand') where
  !sqrtn = sqrt $! fromIntegral n
  !mcNode' = mcNode {simulations = (simulations mcNode) + sqrtn,
                     wins = (wins mcNode) + (w/sqrtn)}
  (!w, !rand') = rollouts n gs rand

-- | Get actions from actions types
mkActions :: MCActions -> [MCAction]
mkActions (InertTerminal _) = []
mkActions (Terminal _ xs) = xs
mkActions (Trunk xs ys) = ys ++ (map action' $ PQ.toAscList xs)
mkActions (Bud solved unsolved) = solved ++ (map f unsolved) where
  f (str, gs) = (str, mkLeaf False gs)


-- | Creates a new leaf for the game (a node that has no children)
mkLeaf :: GameState gs => Bool -> gs -> MCNode
mkLeaf !inert' !gameState = MCNode {gameState, simulations, wins, children} where
  !maybeval = terminal gameState
  !simulations = 0
  !wins = 0
  !children = if isJust $! maybeval
    then if inert'
      then InertTerminal $ fromJust maybeval
      else Terminal (fromJust maybeval) []
    else Bud [] (actions $! gameState)

-- | Makes a trunk out of a completed bud
mkTrunk :: Player -> MCParams -> Value -> [MCAction] -> MCActions
mkTrunk !player !params@(MCParams{exploration, exploitation}) !totalsims !xs =
  maybeTrunk $! partition nonterminal xs where
    !testval = playerBound player params
    nonterminal (_, !MCNode {children = Terminal _ _}) = False
    nonterminal _ = True
    terminalval (_, !MCNode {children = Terminal !realval _}) = realval
    evalfunc (str, !mcNode) = PrioAction {
      action' = (str, mcNode),
      priority = exploitation*(playerValue player)*(avgVal mcNode) +
                 exploration*sqrt((log totalsims)/subsims),
      subsims} where !subsims = simulations mcNode
    !objective = playerObjective player
    maybeTrunk !([], !ys) = Terminal (objective $ map terminalval ys) ys
    maybeTrunk !(!xs, !ys) = if (or $ map ((==testval) . terminalval) ys)
      then Terminal testval (xs ++ ys)
      else Trunk (PQ.fromList $ map evalfunc xs) ys

-- | Advances until the resulting function is called
advanceuntil :: MCParams -> MCNode -> IO (IO MCNode)
advanceuntil params node = if background $ params then do
  mfinish <- newMVar False
  let maxsim' = fromIntegral $ maxsim $ params
      internal cgs = do
        let newrolls = floor ((simulations cgs) / (simsperroll params)) + numrollsI params
            params' = params {uniform=True, numrollsI = newrolls,
                              numrollsSqrt = sqrt $! fromIntegral newrolls}
        hFlush stdout
        rand <- newStdGen
        finish <- readMVar mfinish
        if finish || simulations cgs > maxsim'
          then return cgs
          else
            internal $! multiadvance params' 100 cgs rand
  solver <- async $ internal node
  return $ do
    swapMVar mfinish True
    wait solver
  else return $ return node

-- | Advances until time runs out
timedadvance :: MCParams -> MCNode -> IO MCNode
timedadvance params node = do
  !t <- getCurrentTime
  let maxsim' = fromIntegral $ maxsim $ params
      !st = addUTCTime ((fromIntegral (duration $ params))/1000) t
      internal cgs = do
        let newrolls = floor ((simulations cgs) / (simsperroll params)) + numrollsI params
            params' = params {numrollsI = newrolls, numrollsSqrt = sqrt $! fromIntegral newrolls}
        !ct <- getCurrentTime
        !rand <- newStdGen
        if ct > st || stopcond cgs then return cgs else internal $! multiadvance params' 100 cgs rand
      stopcond (MCNode {children = !(Terminal _ _)}) = True
      stopcond (MCNode {simulations}) = simulations > maxsim'
  internal node
  -- res <- internal node
  -- let sims1 = simulations node
  --     sims2 = simulations res
  --     denom = (fromIntegral $ duration $ params)/1000
  --     persec = (sims2-sims1) / denom
  -- putStr "Performance: "
  -- print ((sims1, sims2, denom), persec)
  -- return res

-- | Perform several advances
multiadvance :: (RandomGen rg) => MCParams -> Int -> MCNode -> rg -> MCNode
multiadvance !params !n !gs !rand  = fst $ (iterate f (gs, rand)) !! n where
  g (!gs', !rand', _) = (gs', rand')
  f (!gs', !rand') = g $! advanceNode params gs' rand'

-- | Advance a node to improve heuristic
advanceNode :: (RandomGen rg) => MCParams -> MCNode -> rg -> (MCNode, rg, Value)
advanceNode !(MCParams {numrollsSqrt})
            !mgs@(MCNode {children = (Terminal !tval _)}) !rand = (mgs, rand, tval*numrollsSqrt)
advanceNode !(MCParams {numrollsSqrt})
            !mgs@(MCNode {children = (InertTerminal !tval)}) !rand = (mgs, rand, tval*numrollsSqrt)
advanceNode !params@(MCParams {numrollsI, numrollsSqrt, inert})
            !mgs@(MCNode {simulations, wins=w, gameState,
                          children = (Bud !post !pre)}) rand =
  (mgs {simulations=simulations', wins = w+val, children = f children'}, rand', val) where
    !simulations' = simulations+numrollsSqrt
    !(!str, !gs) = head pre
    !(!ngs, !rand') = rolloutNode numrollsI (mkLeaf inert gs) rand
    !val = wins ngs
    !player' = player gameState
    !testval = playerBound player' params
    !children' = Bud ((str, ngs) : post) (tail pre)
    f !(Bud !post' []) = mkTrunk player' params simulations' post'
    f !bud@(Bud !post' pre') = if terminalVal ngs == Just testval
      then Terminal testval (post' ++ map (\(!str, !g) -> (str, mkLeaf False g)) pre')
      else bud
advanceNode !params@(MCParams{numrollsSqrt, exploration, exploitation, uniform})
            !mgs@(MCNode {simulations=s, wins=w, gameState,
                          children = !(Trunk !nonterminals !terminals)}) !rand =
  (mgs {simulations = s', wins = w+val, children = f children'}, rand', val) where
    (PrioAction {action' = (!str, !child), subsims = !ss'}, !queue) =
      fromJust $! PQ.extract nonterminals
    !params' = params {uniform = False}
    !exploitation' = if uniform then 0 else exploitation
    !player' = player gameState
    !objective = playerObjective player'
    !s' = s + numrollsSqrt
    (!child', !rand', !val) = advanceNode params' child rand
    !nact = (str, child')
    !children' = case child' of
      MCNode {children = !(Terminal !tval _)} -> if tval == playerBound player' params
        then Terminal tval (nact: (terminals ++ (map action' $ PQ.toAscList queue)))
        else Trunk queue (nact:terminals)
      otherwise -> Trunk (PQ.insert nprio queue) terminals where
        !priority = exploitation*(playerValue player')*(avgVal child') +
                    exploration*sqrt((log s)/ss')
        !nprio = PrioAction {priority, action'=nact, subsims = ss' + numrollsSqrt}
    f ch@(Trunk !q' nt') = if isNothing $! PQ.extract q'
      then Terminal (objective $ map (fromJust . terminalVal . snd) nt') nt'
      else ch
    f ch = ch

-- | Perform a single rollout
rollout :: (GameState a, RandomGen b) => a -> b -> (Value, b)
rollout !gs !rand = if isJust tgs then (fromJust tgs, rand) else rollout gs' rand' where
  !tgs = terminal $! gs
  !ags = actions $! gs
  !nags = numactions $! gs
  !(idx, rand') = randomR (0, nags-1) $! rand
  gs' = snd $! ags !! idx

-- | Perform multiple rollouts
rollouts :: (GameState a, RandomGen b) => Int -> a -> b -> (Value, b)
rollouts 0 _ !rand = (0, rand)
rollouts !n !gs !rand1 = (v + w, rand3) where
  (!v, !rand2) = rollout gs rand1
  (!w, !rand3) = rollouts (n-1) gs rand2

-- | Solve a game with MCTS
mctsSolver :: GameState a => MCParams -> a -> MCSolvedGame
mctsSolver mcParams gs = MCSolvedGame {mcParams, mcNode = mkLeaf False gs}

data MTMCSolvedGame = MTMCSolvedGame {mtNodes :: [MCNode], mtParams :: MCParams}

instance Show MTMCSolvedGame where
  show MTMCSolvedGame {mtNodes = (MCNode {gameState}):_} = show gameState

instance GameState MTMCSolvedGame where
  player MTMCSolvedGame {mtNodes = (MCNode {gameState}):_} = player gameState
  terminal MTMCSolvedGame {mtNodes = (MCNode {gameState}):_} = terminal gameState
  maxdepth MTMCSolvedGame {mtNodes = (MCNode {gameState}):_} = maxdepth gameState
  actions MTMCSolvedGame {mtParams, mtNodes} = map mkSolvedGame $ M.toList solutions where
    solutions = foldr addthread M.empty mtNodes
    addthread node table = foldr appendaction table $ mkActions $ children node
    appendaction (str, node) = M.alter (appendnode node) str
    appendnode node Nothing = Just [node]
    appendnode node (Just nodes) = Just $ node:nodes
    mkSolvedGame (str, mtNodes) = (str, MTMCSolvedGame {mtParams, mtNodes})

instance SolvedGameState MTMCSolvedGame where
  action (MTMCSolvedGame {mtParams, mtNodes}) =
    best <$> mapConcurrently (timedadvance mtParams) mtNodes where
      best nodes = (beststr, MTMCSolvedGame {mtParams, mtNodes = multiact beststr}) where
        multiactions = map (mkActions . children) nodes
        -- this should use lessevil!
        beststr = undefined $ fst $ objective (comparing snd) $ M.toList solutions
        solutions = foldr addthread M.empty multiactions
        addthread actionlist table = foldr aggaction table actionlist
        aggaction (str, node) = M.alter (aggnode node) str
        aggnode node Nothing = Just $ nodeValue node
        aggnode node (Just preval) = Just $ combineValues preval $ nodeValue node
        multiact str = map (fromJust . lookup str) multiactions
      mcplayer (MCNode {gameState}) = player gameState
      objective = playerObjectiveBy $! mcplayer $ head mtNodes
  think (MTMCSolvedGame {mtParams, mtNodes}) = do
    funcs <- mapM (advanceuntil mtParams) mtNodes
    return $ do
      mtNodes <- sequence funcs
      return MTMCSolvedGame {mtParams, mtNodes}

-- | Solve a game with Multithreaded MCTS
mtmctsSolver :: GameState a => Int -> MCParams -> a -> MTMCSolvedGame
mtmctsSolver n mtParams gs = MTMCSolvedGame {mtParams, mtNodes = map (mkLeaf False) $ replicate n gs}

-- | returns the best value regardless of terminal states
lessevilMCTS :: GameState a => MCParams -> a -> [String] -> IO String
lessevilMCTS params gs strs = do
  MCNode {children = (Trunk nonterminals _)} <- timedadvance (params {inert = True}) $ mkLeaf True gs
  let valids = filter (\(str, _) -> elem str strs) $ map action' $ PQ.toAscList nonterminals
      objective = playerObjectiveBy (player gs) (comparing $ nodeValue . snd)
  return $ fst $ objective valids

-- | returns the best value regardless of terminal states
lessevilMTMCTS :: GameState a => Int -> MCParams -> a -> [String] -> IO String
lessevilMTMCTS = undefined
