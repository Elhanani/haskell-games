{-# LANGUAGE ExistentialQuantification
           , NamedFieldPuns
           , FlexibleContexts
           , AllowAmbiguousTypes
           , BangPatterns
           #-}

module Minmax (minmaxSolver, ordMinmaxSolver, hashMinmaxSolver, singleLRUMinmaxSolver) where

import SolverDefs
import Control.Monad.State
import Control.Monad.ST
import Data.Maybe
import qualified Data.Map as M
import Cache

-- | Basically adds a value to each game state
data MMNode gs = MMNode {gameState :: gs, value :: Value}

data MMSolvedGame = forall gs. (GameState gs) =>
  MMSolvedGame {mmNode :: MMNode gs, solver :: gs -> MMSolvedGame}

instance Show MMSolvedGame where
  show (MMSolvedGame {mmNode = MMNode {gameState}}) = show gameState

instance GameState MMSolvedGame where
  player (MMSolvedGame {mmNode = MMNode {gameState}}) = player gameState
  terminal (MMSolvedGame {mmNode = MMNode {gameState}}) = terminal gameState
  maxdepth (MMSolvedGame {mmNode = MMNode {gameState}}) = maxdepth gameState
  actions (MMSolvedGame {mmNode = MMNode {gameState}, solver}) =
    map internal $ actions gameState where
      internal (str, gs) = (str, solver gs)

instance SolvedGameState MMSolvedGame where
  action g@(MMSolvedGame {mmNode = MMNode {value=v1}}) =
    randomAction $ filter eqval $ actions g where
      eqval (_, MMSolvedGame {mmNode = MMNode {value=v2}}) = v2 == v1

-- | An unbound minmax solver with Cache monad
minmax :: (GameState gs, Cache m r gs (MMNode gs)) => m r -> gs -> m (MMNode gs)
minmax !mref !gs = do
  (retval, _) <- minmax' mref gs
  return retval

minmax' :: (GameState gs, Cache m r gs (MMNode gs)) => m r -> gs -> m (MMNode gs, r)
minmax' !mref !gs = do
  !uref <- mref
  !ret <- innermm uref gs
  return (ret, uref)
  where
    innermm ref gameState = do
      !mval <- readCache ref gameState
      if isJust mval then return $ fromJust mval else
        let !tval = terminal gameState in if isJust tval
          then return MMNode {gameState, value = fromJust tval} else do
            !actions' <- mapM internal $ actions gameState
            let !objective = playerObjective $! player gameState
                !cval = objective $! map (value . snd) $ actions'
                !ngs = MMNode {gameState, value = cval}
            writeCache ref gameState ngs
            return ngs
      where
        internal (str, !ogs) = do
          !ngs <- innermm ref ogs
          return (str, ngs)

-- | A simple unbound minmax solver
minmaxSolver :: (GameState a) => a -> MMSolvedGame
minmaxSolver g = MMSolvedGame {mmNode = runNoCache minmax g, solver = minmaxSolver}

-- | A memoized unbound minmax solver
ordMinmaxSolver :: (GameState gs, Ord gs) => gs -> MMSolvedGame
ordMinmaxSolver = ordMinmaxSolver' initial

ordMinmaxSolver' :: (GameState gs, Ord gs) => MapState gs (MMNode gs) -> gs -> MMSolvedGame
ordMinmaxSolver' pstate g = MMSolvedGame {mmNode, solver} where
  (mmNode, nstate) = mapSolver minmax pstate g
  solver = ordMinmaxSolver' nstate

-- | A memoized unbound minmax solver
hashMinmaxSolver :: (GameState gs, Hashable gs, Eq gs) => gs -> MMSolvedGame
hashMinmaxSolver = hashMinmaxSolver' initial

hashMinmaxSolver' :: (GameState gs, Hashable gs, Eq gs) =>
  HashMapState gs (MMNode gs) -> gs -> MMSolvedGame
hashMinmaxSolver' pstate g = MMSolvedGame {mmNode, solver} where
  (mmNode, nstate) = hashMapSolver minmax pstate g
  solver = hashMinmaxSolver' nstate

-- | A memoized unbound minmax solver
singleLRUMinmaxSolver :: (GameState gs, Hashable gs, Eq gs) => Int -> gs -> MMSolvedGame
singleLRUMinmaxSolver = singleLRUMinmaxSolver' . initialSingleHashRef (\_ _ -> True)

singleLRUMinmaxSolver' :: (GameState gs, Hashable gs, Eq gs) =>
  SingleHashState gs (MMNode gs) -> gs -> MMSolvedGame
singleLRUMinmaxSolver' pref g = MMSolvedGame {mmNode, solver} where
  (mmNode, nref) = runST $ singleHashCacheST pref minmax' g
  solver = singleLRUMinmaxSolver' nref
