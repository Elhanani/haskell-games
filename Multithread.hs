{-# LANGUAGE ExistentialQuantification, BangPatterns, NamedFieldPuns, RankNTypes #-}

module Multithread where

import SolverDefs

data MultiSolvedGame = forall gs. (SolvedGameState gs) => Multi {universes :: [gs]}

rootParallelSolver :: forall gs. (SolvedGameState gs) =>
                      ([gs] -> [(String, gs)]) -> [gs] -> MultiSolvedGame
rootParallelSolver = undefined
