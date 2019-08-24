module Main where

import Connect4
import TTT
import UTT
import NimEuclidean
import NimFibonacci
import Nim
import Minmax
import qualified MCTS2 as M2
import qualified MCTS as M1

-- s1 = M1.mctsSolver $ M1.defaultMCParams {M1.duration=1000, M1.background = False}
s2 = M2.mctsSolver $ M2.defaultMCParams {M2.duration=300, M2.background = False}

main = do
  putStrLn ""
  interaction connect4game s2 s2
  -- humanInteraction connect4game $ mctsSolver $ defaultMCParams {duration=1000}
  -- humanInteraction ultimateTTT $ mtmctsSolver 3 $ defaultMCParams {duration=1000}
  -- humanInteraction (nimGame [1, 2, 3, 4, 5, 6, 7]) nimSolver
  -- humanInteraction (nimEuclideanGame 55 89) nimEuclideanSolver
  -- humanInteraction (nimFibonacciGame 89) nimFibonacciSolver
  -- interaction tictactoe hashMinmaxSolver ordMinmaxSolver
