{-# LANGUAGE ExistentialQuantification
           , NamedFieldPuns
           , FlexibleContexts
           , AllowAmbiguousTypes
           , BangPatterns
           #-}

module Minmax (minmaxSolver, ordMinmaxSolver, hashMinmaxSolver) where

import SolverDefs
import Control.Monad.State
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

-- -- | Original minmax solver
-- minmaxSolver :: (GameState a) => a -> MMSolvedGame
-- minmaxSolver gameState = MMSolvedGame {gameState, value = value', actions'} where
--   internal (str, ngs) = (str, minmaxSolver ngs)
--   actions' = map internal $ actions gameState
--   tval = terminal gameState
--   objective = playerObjective $! player gameState
--   value' = if isJust $ tval then fromJust tval else
--     objective $ map (value . snd) $ actions'

-- | An unbound minmax solver with Cache monad
minmax :: (GameState gs, Cache m gs MMSolvedGame) => gs -> m MMSolvedGame
minmax !gameState = do
  !mval <- readCache gameState
  if isJust mval then return $ fromJust mval else
    let !tval = terminal gameState in if isJust tval
      then return MMSolvedGame {gameState, value = fromJust tval, actions' = []} else do
        !actions' <- mapM internal $ actions gameState
        let !objective = playerObjective $! player gameState
            !cval = objective $! map (value . snd) $ actions'
            !ngs = MMSolvedGame {gameState, value = cval, actions'}
        writeCache gameState ngs
        return ngs
  where internal (str, !ogs) = do
          !ngs <- minmax ogs
          return (str, ngs)

-- | An unbound minmax solver with ST monad

-- minmaxST :: (GameState gs, STHashRef ref gs MMSolvedGame) => ref -> gs -> MMSolvedGame
-- minmaxST = runST $ do
--   return undefined

-- | A simple unbound minmax solver
minmaxSolver :: (GameState a) => a -> MMSolvedGame
minmaxSolver g = runNoCache minmax g


-- | A memoized unbound minmax solver
ordMinmaxSolver :: (GameState gs, Ord gs) => gs -> MMSolvedGame
ordMinmaxSolver g = runMapCache minmax g

-- | A memoized unbound minmax solver
hashMinmaxSolver :: (GameState gs, Hashable gs, Eq gs) => gs -> MMSolvedGame
hashMinmaxSolver g = runHashMapCache minmax g
