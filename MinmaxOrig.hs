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
data MMSolvedGame = forall gs. (GameState gs) =>
  MMSolvedGame {gameState :: gs, value :: Value, actions' :: [(String, MMSolvedGame)]}

instance Show MMSolvedGame where
  show (MMSolvedGame {gameState}) = show gameState

instance GameState MMSolvedGame where
  player (MMSolvedGame {gameState}) = player gameState
  terminal (MMSolvedGame {gameState}) = terminal gameState
  maxdepth (MMSolvedGame {gameState}) = maxdepth gameState
  actions = actions'

instance SolvedGameState MMSolvedGame where
  action g = randomAction $ filter ((== value g) . value . snd) $ actions g

-- | An unbound minmax solver with Cache monad
minmax :: (GameState gs, Cache m r gs MMSolvedGame) => m r -> gs -> m MMSolvedGame
minmax !mref !gs = do
  !uref <- mref
  minmax' uref gs
  where
    minmax' ref gameState = do
      !mval <- readCache ref gameState
      if isJust mval then return $ fromJust mval else
        let !tval = terminal gameState in if isJust tval
          then return MMSolvedGame {gameState, value = fromJust tval, actions' = []} else do
            !actions' <- mapM internal $ actions gameState
            let !objective = playerObjective $! player gameState
                !cval = objective $! map (value . snd) $ actions'
                !ngs = MMSolvedGame {gameState, value = cval, actions'}
            writeCache ref gameState ngs
            return ngs
      where
        internal (str, !ogs) = do
          !ngs <- minmax' ref ogs
          return (str, ngs)

-- | A simple unbound minmax solver
minmaxSolver :: (GameState a) => a -> MMSolvedGame
minmaxSolver g = runNoCache minmax g


-- | A memoized unbound minmax solver
ordMinmaxSolver :: (GameState gs, Ord gs) => gs -> MMSolvedGame
ordMinmaxSolver g = runMapCache minmax g

-- | A memoized unbound minmax solver
hashMinmaxSolver :: (GameState gs, Hashable gs, Eq gs) => gs -> MMSolvedGame
hashMinmaxSolver g = runHashMapCache minmax g

-- | A memoized unbound minmax solver
singleLRUMinmaxSolver :: (GameState gs, Hashable gs, Eq gs) => Int -> gs -> MMSolvedGame
singleLRUMinmaxSolver size g = runST $ singleHashCacheST size (const $ const True) minmax g where
