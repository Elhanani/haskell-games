module Main where

import Connect4
import TTT
import UTT
import NimEuclidean
import NimFibonacci
import Nim
import Minmax
import MCTS

main = do
  putStrLn ""
  humanInteraction connect4game $ mtmctsSolver 3 $ defaultMCParams {duration=1000}
  -- humanInteraction ultimateTTT $ mtmctsSolver 3 $ defaultMCParams {duration=1000}
  -- humanInteraction (nimGame [1, 2, 3, 4, 5, 6, 7]) nimSolver
  -- humanInteraction (nimEuclideanGame 55 89) nimEuclideanSolver
  -- humanInteraction (nimFibonacciGame 89) nimFibonacciSolver
  -- interaction tictactoe hashMinmaxSolver ordMinmaxSolver
