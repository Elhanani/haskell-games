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
import qualified PQueueQuick as PQ
import System.IO

import Debug.Trace

-- | exploration/expolitation are coefficients for the UCB funtion
--   alpha, beta are values that will stop the search
--   duration is the amount of time in milliseconds to search
--   maxsim is the maximum count of simulations
--   background is True when we allow thinking in the background
--   uniform is True when we want to explore and not to exploit only for the first node
data MCParams = MCParams
  {exploitation :: {-# UNPACK #-} !Value, exploration :: {-# UNPACK #-} !Value,
   alpha :: {-# UNPACK #-} !Value, beta :: {-# UNPACK #-} !Value,
   duration ::  {-# UNPACK #-} !Int, maxsim :: {-# UNPACK #-} !Int,
   numrollsI :: {-# UNPACK #-} !Int, numrollsF :: {-# UNPACK #-} !Value,
   simsperroll :: {-# UNPACK #-} !Value, background :: !Bool, uniform :: !Bool}

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
data MCActions = Terminal (Value, [MCAction])
               | forall gs. GameState gs => Bud ([MCAction], [(String, gs)])
               | Trunk (PQ.PQueue PrioAction, [MCAction])

defaultMCParams :: MCParams
defaultMCParams = MCParams {exploitation = 1, exploration = sqrt 2,
                            alpha = (-1), beta = 1,
                            duration = 1000, maxsim = 100000000, background = True,
                            numrollsI = 1, numrollsF = 1, simsperroll = 100000,
                            uniform = False}

playerBound :: Player -> MCParams -> Value
playerBound Maximizer = beta
playerBound Minimizer = alpha

playerObjectiveBy :: (Foldable t) => Player -> (a -> a -> Ordering) -> t a -> a
playerObjectiveBy Maximizer = maximumBy
playerObjectiveBy Minimizer = minimumBy

-- | Getter function from a terminal
terminalVal :: MCNode -> Maybe Value
terminalVal !(MCNode {children = !(Terminal (!v, _))}) = Just v
terminalVal _ = Nothing

winRate :: MCNode -> Value
winRate !mcNode = (wins $! mcNode) / (simulations $! mcNode)

instance Show MCSolvedGame where
  show MCSolvedGame {mcNode = MCNode {gameState}} = show gameState

instance GameState MCSolvedGame where
  player MCSolvedGame {mcNode = MCNode {gameState}} = player gameState
  terminal MCSolvedGame {mcNode = MCNode {gameState}} = terminal gameState
  maxdepth MCSolvedGame {mcNode = MCNode {gameState}} = maxdepth gameState
  actions MCSolvedGame {mcParams, mcNode = MCNode {children}} =
    map mkSolvedGame $ mkActions children where
      mkActions :: MCActions -> [MCAction]
      mkActions (Terminal (_, xs)) = xs
      mkActions (Trunk (xs, ys)) = ys ++ (map action' $ PQ.toAscList xs)
      mkActions (Bud (solved, unsolved)) = solved ++ (map f unsolved) where
        f (str, gs) = (str, mkLeaf gs)
      mkSolvedGame (str, mcNode) = (str, MCSolvedGame {mcParams, mcNode})

instance SolvedGameState MCSolvedGame where
  action mcsg@(MCSolvedGame {mcParams, mcNode = MCNode {gameState}}) =
    (best . mcNode) <$> timedadvance mcsg where
      best (MCNode {children = Terminal (v, terminals)}) = toMCSolvedGame $
        head $ filter ((== Just v) . terminalVal . snd) terminals
      best (MCNode {children = Trunk (nonterminals, _)}) = toMCSolvedGame $
        action' $ objective (comparing $! winRate . snd . action') $ PQ.toAscList nonterminals
      objective = playerObjectiveBy $! player gameState
      toMCSolvedGame (str, mcNode) = (str, MCSolvedGame {mcParams, mcNode})
  think = advanceuntil

-- | Perform rollouts and update the node
rolloutNode :: RandomGen rg => Int -> MCNode -> rg -> (MCNode, rg)
rolloutNode !n !mcNode@(MCNode {gameState = gs}) !rand = (mcNode', rand') where
  !sqrtn = sqrt $! fromIntegral n
  !mcNode' = mcNode {simulations = (simulations mcNode) + sqrtn,
                     wins = (wins mcNode) + (w/sqrtn)}
  (!w, !rand') = rollouts n gs rand

-- | Creates a new leaf for the game (a node that has no children)
mkLeaf :: GameState gs => gs -> MCNode
mkLeaf !gameState = MCNode {gameState, simulations, wins, children} where
  !maybeval = terminal gameState
  !simulations = 0
  !wins = 0
  !children = if isJust $! maybeval
    then Terminal (fromJust maybeval, [])
    else Bud ([], actions $! gameState)

-- | Makes a trunk out of a completed bud
mkTrunk :: Player -> MCParams -> Value -> [MCAction] -> MCActions
mkTrunk !player !params@(MCParams{exploration, exploitation}) !totalsims !xs =
  maybeTrunk $! partition nonterminal xs where
    !testval = playerBound player params
    nonterminal (_, !MCNode {children = Terminal _}) = False
    nonterminal _ = True
    terminalval (_, !MCNode {children = Terminal (!realval, _)}) = realval
    evalfunc (str, !mcNode) = PrioAction {
      action' = (str, mcNode),
      priority = exploitation*(playerValue player)*(winRate mcNode) +
                 exploration*sqrt((log totalsims)/subsims),
      subsims} where !subsims = simulations mcNode
    !objective = playerObjective player
    maybeTrunk !([], !ys) = Terminal (objective $ map terminalval ys, ys)
    maybeTrunk !(!xs, !ys) = if (or $ map ((==testval) . terminalval) ys)
      then Terminal (testval, xs ++ ys)
      else Trunk (PQ.fromList $ map evalfunc xs, ys)


-- change the simsperroll inside the internal loop
-- | Advances until the resulting function is called
advanceuntil :: MCSolvedGame -> IO (IO MCSolvedGame)
advanceuntil mcsg@(MCSolvedGame {mcNode, mcParams}) = if background $ mcParams then do
  mfinish <- newMVar False
  let maxsim' = fromIntegral $ maxsim $ mcParams
      internal cgs = do
        let newrolls = floor ((simulations cgs) / (simsperroll mcParams)) + numrollsI mcParams
            params' = mcParams {uniform=True, numrollsI = newrolls, numrollsF = fromIntegral newrolls}
        hFlush stdout
        rand <- newStdGen
        finish <- readMVar mfinish
        if finish || simulations cgs > maxsim'
          then return mcsg {mcNode=cgs}
          else
            internal $! multiadvance params' 100 cgs rand
  solver <- async $ internal mcNode
  return $ do
    swapMVar mfinish True
    wait solver
  else return $ return mcsg

-- change the simsperroll inside the internal loop
-- | Advances until time runs out
timedadvance :: MCSolvedGame -> IO MCSolvedGame
timedadvance mcsg@(MCSolvedGame {mcNode, mcParams}) = do
  !t <- getCurrentTime
  let maxsim' = fromIntegral $ maxsim $ mcParams
      !st = addUTCTime ((fromIntegral (duration $ mcParams))/1000) t
      internal cgs = do
        let newrolls = floor ((simulations cgs) / (simsperroll mcParams)) + numrollsI mcParams
            params' = mcParams {numrollsI = newrolls, numrollsF = fromIntegral newrolls}
        !ct <- getCurrentTime
        !rand <- newStdGen
        if ct > st || stopcond cgs then return cgs else internal $! multiadvance params' 100 cgs rand
      stopcond (MCNode {children = !(Terminal _)}) = True
      stopcond (MCNode {simulations}) = simulations > maxsim'
  res <- internal mcNode
  -- let sims1 = simulations mcNode
  --     sims2 = simulations res
  --     denom = (fromIntegral $ duration $ mcParams)/1000
  --     persec = (sims2-sims1) / denom
  -- putStr "Performance: "
  -- print ((sims1, sims2, denom), persec)
  return mcsg {mcNode = res}

-- | Perform several advances
multiadvance :: (RandomGen rg) => MCParams -> Int -> MCNode -> rg -> MCNode
multiadvance !params !n !gs !rand  = fst $ (iterate f (gs, rand)) !! n where
  g (!gs', !rand', _) = (gs', rand')
  f (!gs', !rand') = g $! advanceNode params gs' rand'

-- | Advance a node to improve heuristic
advanceNode :: (RandomGen rg) => MCParams -> MCNode -> rg -> (MCNode, rg, Value)
advanceNode _ !mgs@(MCNode {children = (Terminal (!tval, _))}) !rand = (mgs, rand, tval)
advanceNode !params@(MCParams {numrollsI, numrollsF})
            !mgs@(MCNode {simulations, wins=w, gameState,
                          children = (Bud (!post, !pre))}) rand =
  (mgs {simulations=simulations', wins = w+val, children = f children'}, rand', val) where
    !simulations' = simulations+numrollsF
    !(!str, !gs) = head pre
    !(!ngs, !rand') = rolloutNode numrollsI (mkLeaf gs) rand
    !val = wins ngs
    !player' = player gameState
    !testval = playerBound player' params
    !children' = Bud ((str, ngs) : post, tail pre)
    f !(Bud (!post', [])) = mkTrunk player' params simulations' post'
    f !bud@(Bud (!post', pre')) = if terminalVal ngs == Just testval
      then Terminal (testval, post' ++ map (\(!str, !g) -> (str, mkLeaf g)) pre')
      else bud
advanceNode !params@(MCParams{numrollsF, exploration, exploitation, uniform})
            !mgs@(MCNode {simulations=s, wins=w, gameState,
                          children = !(Trunk (!nonterminals, !terminals))}) !rand =
  (mgs {simulations = s', wins = w+val, children = f children'}, rand', val) where
    (PrioAction {action' = (!str, !child), subsims = !ss'}, !queue) =
      fromJust $! PQ.extract nonterminals
    !params' = params {uniform = False}
    !exploitation' = if uniform then 0 else exploitation
    !player' = player gameState
    !objective = playerObjective player'
    !s' = s + numrollsF
    (!child', !rand', !val) = advanceNode params' child rand
    !nact = (str, child')
    !children' = case child' of
      MCNode {children = !(Terminal (!tval, _))} -> if tval == playerBound player' params
        then Terminal (tval, nact: (terminals ++ (map action' $ PQ.toAscList queue)))
        else Trunk (queue, nact:terminals)
      otherwise -> Trunk (PQ.insert nprio queue, terminals) where
        !priority = exploitation*(playerValue player')*(winRate child') +
                    exploration*sqrt((log s)/ss')
        !nprio = PrioAction {priority, action'=nact, subsims = ss' + numrollsF}
    f ch@(Trunk (!q', nt')) = if isNothing $! PQ.extract q'
      then Terminal (objective $ map (fromJust . terminalVal . snd) nt', nt')
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
mctsSolver mcParams gs = MCSolvedGame {mcParams, mcNode = mkLeaf gs}

combineMCTS :: [MCSolvedGame] -> [(String, MCSolvedGame)]
combineMCTS = undefined





-- For performace measuring only!

timedrollouts :: (GameState a) => a -> UTCTime -> IO (Value, Int)
timedrollouts gs st = do
  let inc = 100
      runtil (val, attempts) = do
        !rand <- newStdGen
        let !cval = val + (fst $ rollouts inc gs rand)
            !cattempts = attempts + inc
        t <- getCurrentTime
        if t > st then return (cval, cattempts) else runtil (cval, cattempts)
  ret <- runtil (0, 0)
  return ret

multitimed :: (GameState a) => a -> Int -> IO [(Value, Int)]
multitimed gs dur = do
  t <- getCurrentTime
  -- n <- getNumCapabilities
  let st = addUTCTime ((fromIntegral dur)/1000) t
  mapConcurrently (const $ timedrollouts gs st) [1..2] -- [1..4] [1..n-1]

singlemed :: (GameState a) => a -> Int -> IO (Value, Int)
singlemed gs dur = do
  t <- getCurrentTime
  let st = addUTCTime ((fromIntegral dur)/1000) t
  timedrollouts gs st
