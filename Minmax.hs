{-# LANGUAGE ExistentialQuantification, NamedFieldPuns #-}

module Minmax (minmaxSolver, memoMinmaxSolver) where

import SolverDefs
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M

data MMSolvedGame = forall gs. (GameState gs) =>
  MMSolvedGame {gameState :: gs, value :: Value, actions' :: [(String, MMSolvedGame)]}

instance Show MMSolvedGame where
  show (MMSolvedGame {gameState}) = show gameState

instance GameState MMSolvedGame where
  firstplayer (MMSolvedGame {gameState}) = firstplayer gameState
  terminal (MMSolvedGame {gameState}) = terminal gameState
  maxdepth (MMSolvedGame {gameState}) = maxdepth gameState
  actions = actions'

instance SolvedGameState MMSolvedGame where
  action g = randomAction $ filter ((== value g) . value . snd) $ actions g

minmaxSolver :: (GameState a) => a -> MMSolvedGame
minmaxSolver gameState = MMSolvedGame {gameState, value = value', actions'} where
  internal (str, ngs) = (str, minmaxSolver ngs)
  actions' = map internal $ actions gameState
  tval = terminal gameState
  objective = if firstplayer gameState then maximum else minimum
  value' = if isJust $ tval then fromJust tval else
    objective $ map (value . snd) $ actions'

memominmax :: (GameState gs, Ord gs) => gs -> State (M.Map gs MMSolvedGame) MMSolvedGame
memominmax gameState = do
  memo1 <- get
  let mval = M.lookup gameState memo1
  if isJust mval then return $ fromJust mval else
    let tval = terminal gameState in if isJust tval
      then return MMSolvedGame {gameState, value = fromJust tval, actions' = []} else do
        actions' <- mapM internal $ actions gameState
        memo2 <- get
        let objective = if firstplayer gameState then maximum else minimum
            cval = objective $ map (value . snd) $ actions'
            ngs = MMSolvedGame {gameState, value = cval, actions'}
        put $ M.insert gameState ngs memo2
        return ngs
  where internal (str, ogs) = do
          ngs <- memominmax ogs
          return (str, ngs)

memoMinmaxSolver :: (GameState gs, Ord gs) => gs -> MMSolvedGame
memoMinmaxSolver g = fst $ runState (memominmax g) M.empty