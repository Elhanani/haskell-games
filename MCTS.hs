{-# LANGUAGE ExistentialQuantification, BangPatterns, NamedFieldPuns, RankNTypes #-}

module MCTS where

import SolverDefs
import Data.Maybe
import System.Random
import Data.Time
import Data.List
import Control.Concurrent.Async
import qualified PQueueQuick as PQ

-- Improvements:
-- 1. Root paralellization (with different combinators: "Best" / "Sum" / etc)
-- 2. Rollout paralellization
-- 2. Transpositions (/memoization per single run, to make the graph into a dag)
-- 3. Heuristics
-- 4. RAVE / EMAF
-- 5. Ability to switch to a different solver (ab?) on one thread for the end game


data MCSolvedGame = forall gs. (GameState gs) =>
    MCSolvedGame {gameState :: gs,
                  simulations :: Value,
                  wins :: Value,
                  children :: MCActions,
                  params :: MCParams
                 }

newtype MCAction = MCAction {unMCAction :: (Value, (String, MCSolvedGame))}

instance Eq MCAction where
  mc1 == mc2 = (fst $! snd $! unMCAction $! mc2) == (fst $! snd $! unMCAction $! mc1)

instance Ord MCAction where
  compare mc1 mc2 = compare (fst $! unMCAction $! mc2) (fst $! unMCAction $! mc1)

data MCActions = Terminal (Value, [MCAction])
               | forall gs. GameState gs => Bud (Value, [MCAction], [(String, gs)])
               | Trunk (PQ.PQueue MCAction, [MCAction])

data MCParams = MCParams
  {evalfunc :: Bool -> Value -> Value -> Value -> Value,
   alpha :: Value, beta :: Value,
   duration :: Int, maxsim :: Int, background :: Bool}

instance Show MCSolvedGame where
  show (MCSolvedGame {gameState}) = show gameState

instance GameState MCSolvedGame where
  terminal (MCSolvedGame {gameState}) = terminal gameState
  firstplayer (MCSolvedGame {gameState}) = firstplayer gameState
  maxdepth (MCSolvedGame {gameState}) = maxdepth gameState
  actions (MCSolvedGame {children = Terminal (_, xs)}) = map (snd . unMCAction) xs
  actions (MCSolvedGame {children = Trunk (xs, ys)}) = map (snd . unMCAction) ((PQ.toAscList xs) ++ ys)
  actions (MCSolvedGame {gameState, children = Bud (_, solved, unsolved), params=p}) =
    (map (snd . unMCAction) solved) ++ (map f unsolved) where
      f (str, gs) = (str, mkLeaf' p gs)

instance SolvedGameState MCSolvedGame where
  action mgs = best <$> timedadvance mgs where
    best (MCSolvedGame {children = Terminal (v, terminals)}) =
      snd $ unMCAction $ head $ filter ((==v) . fst . unMCAction) terminals
    best (MCSolvedGame {children = Trunk (nonterminals, _)}) =
      snd $ unMCAction $ objective f $ PQ.toAscList nonterminals
    f (MCAction (_, (_, MCSolvedGame {wins=w1, simulations=s1})))
      (MCAction (_, (_, MCSolvedGame {wins=w2, simulations=s2}))) =
        compare (w1/s1) (w2/s2)
    objective = if firstplayer mgs then maximumBy else minimumBy

mkLeaf :: (GameState gs, RandomGen rg) => MCParams -> rg -> gs -> (MCSolvedGame, rg)
mkLeaf !params !rand !gameState =
  (MCSolvedGame {gameState, simulations, wins, children, params}, rand') where
    !maybeval = terminal gameState
    !simulations = 1
    !(!wins, !children, !rand') = if isJust $! maybeval
      then let justval = fromJust maybeval in (justval, Terminal (justval, []), rand)
      else (wins', Bud (fromIntegral $ numactions $! gameState, [], actions $! gameState), rand'') where
        !(!wins', !rand'') = rollout gameState $! rand

mkLeaf' :: GameState gs => MCParams -> gs -> MCSolvedGame
mkLeaf' !params !gameState =
  MCSolvedGame {gameState, simulations, wins, children, params} where
    !maybeval = terminal gameState
    !simulations = 0
    !wins = 0
    !children = if isJust $! maybeval
      then Terminal (fromJust maybeval, [])
      else Bud (fromIntegral $ numactions $! gameState, [], actions $! gameState)

mkTrunk :: Bool -> Value -> [MCAction] -> MCActions
mkTrunk !first !testval !xs = maybeTrunk $! partition f xs where
  f (MCAction (_, (_, !MCSolvedGame {children = Terminal _}))) = False
  f _ = True
  g (MCAction (_, (_, !MCSolvedGame {children = Terminal (realval, _)}))) = realval
  !objective = if first then maximum else minimum
  maybeTrunk !([], !ys) = Terminal (objective $ map g ys, ys)
  maybeTrunk !(!xs, !ys) = if (or $ map ((==testval) . g) ys)
    then Terminal (testval, xs ++ ys)
    else Trunk (PQ.fromList xs, ys)

timedadvance :: MCSolvedGame -> IO MCSolvedGame
timedadvance mgs = do
  !t <- getCurrentTime
  let !maxsim' = fromIntegral $ maxsim $ params mgs
      !st = addUTCTime ((fromIntegral (duration $ params mgs))/1000) t
      internal cgs = do
        !ct <- getCurrentTime
        !rand <- newStdGen
        if ct > st || stopcond cgs then return cgs else internal $! multiadvance 1000 cgs rand
      stopcond (MCSolvedGame {children = !(Terminal _)}) = True
      stopcond (MCSolvedGame {simulations}) = simulations > maxsim'
  internal mgs
  -- res <- internal mgs
  -- let sims1 = simulations mgs
  --     sims2 = simulations res
  --     denom = (fromIntegral $ duration $ params mgs)/1000
  --     persec = (sims2-sims1) / denom
  -- putStr "Performance: "
  -- print ((sims1, sims2, denom), persec)
  -- return res

multiadvance :: (RandomGen rg) => Int -> MCSolvedGame -> rg -> MCSolvedGame
multiadvance n gs rand  = fst $ (iterate f (gs, rand)) !! n where
  g (gs', rand', _) = (gs', rand')
  f (gs', rand') = g $ advanceNode gs' rand'

advanceNode :: (RandomGen rg) => MCSolvedGame -> rg -> (MCSolvedGame, rg, Value)
advanceNode !mgs@(MCSolvedGame {children = (Terminal (tval, _))}) rand = (mgs, rand, tval)
advanceNode !mgs@(MCSolvedGame {simulations, wins=w, gameState,
                                children = (Bud (len, post, pre)),
                                params = p@(MCParams {evalfunc, alpha, beta})}) rand =
  (mgs {simulations = simulations+1, wins = w+val, children = f children'}, rand', val) where
    !(!str, !gs) = head pre
    !(!ngs, !rand') = mkLeaf p rand gs
    !val = wins ngs
    !first = firstplayer gameState
    !eval = fromMaybe (evalfunc first val 1 len) (terminalVal $ children ngs)
    !nact = MCAction $! (eval, (str, ngs))
    !children' = Bud (len, nact : post, tail pre)
    f !(Bud (_, !post', [])) = mkTrunk first (if first then beta else alpha) post'
    f !bud = bud
advanceNode !mgs@(MCSolvedGame {simulations=s, wins=w, gameState,
                                children = (Trunk (nonterminals, terminals)),
                                params = (MCParams {evalfunc, alpha, beta})}) rand =
  (mgs {simulations = s', wins = w+val, children = f children'}, rand', val) where
    (MCAction (_, (!str, !child)), !queue) = fromJust $ PQ.extract nonterminals
    !first = firstplayer gameState
    !objective = if first then maximum else minimum
    !s' = s+1
    (!child', !rand', !val) = advanceNode child rand
    !children' = case child' of
      MCSolvedGame {children = (Terminal (tval, _))} -> if tval == (if first then beta else alpha)
        then Terminal (tval, (MCAction (tval, (str, child')):terminals) ++ PQ.toAscList queue)
        else Trunk (queue, MCAction (tval, (str, child')):terminals)
      otherwise -> Trunk (PQ.insert nact queue, terminals) where
        !eval = fromMaybe (evalfunc first (wins child') (simulations child) s')
                          (terminalVal $ children child')
        !nact = MCAction (eval ,(str, child'))
    f ch@(Trunk (!q', nt')) = if isNothing $ PQ.extract q'
      then Terminal (objective $ map (fst . unMCAction) nt', nt')
      else ch
    f ch = ch

terminalVal :: MCActions -> Maybe Value
terminalVal !(Terminal (!v, _)) = Just v
terminalVal _ = Nothing

ucb1 :: Value -> Bool -> Value -> Value -> Value -> Value
ucb1 !c !first !w !n !nn = sqrt (c*(log nn)/n) + (w/n)*(if first then 1 else -1)

rollout :: (GameState a, RandomGen b) => a -> b -> (Value, b)
rollout !gs !rand = if isJust tgs then (fromJust tgs, rand) else rollout gs' rand' where
  !tgs = terminal $! gs
  !ags = actions $! gs
  !nags = numactions $! gs
  !(idx, rand') = randomR (0, nags-1) $! rand
  gs' = snd $! ags !! idx

rollouts :: (GameState a, RandomGen b) => Int -> a -> b -> Value
rollouts 0 _ _ = 0
rollouts n gs rand = v + (rollouts (n-1) gs rand') where
  (v, rand') = rollout gs rand

mctsSolver :: GameState a => MCParams -> a -> MCSolvedGame
mctsSolver params gs = mkLeaf' params gs



-- For performace measuring only!

timedrollouts :: (GameState a) => a -> UTCTime -> IO (Value, Int)
timedrollouts gs st = do
  let inc = 100
      runtil (val, attempts) = do
        !rand <- newStdGen
        let !cval = val + rollouts inc gs rand
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
