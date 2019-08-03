module Nim (nimGame, nimSolver) where

import SolverDefs
import Data.List
import Data.Bits
import qualified Data.Set as Set

newtype NimGame = Nim (Player, [Integer])

instance Show NimGame where
  show (Nim (player', piles)) = (unwords $ map show piles) ++ message where
    message = "\nIt's " ++ pl ++ " player's turn\n"
    pl = case player' of
      Maximizer -> "first"
      Minimizer -> "second"

instance GameState NimGame where
  player (Nim (player', _)) = player'
  terminal (Nim (player', piles)) = if null piles then Just (playerValue player') else Nothing
  actions (Nim (player', piles)) = [(movename (n, m), newng n m) | n <- uniq piles, m <- [0..n-1]] where
    uniq = Set.toList . Set.fromList
    newng n m = Nim (otherPlayer player', filter (/= 0) $ a ++ [m] ++ b) where
      (a, b) = fmap (drop 1) $ break (n ==) piles

movename :: (Integer, Integer) -> String
movename (n, m) = (show n) ++ "-" ++ (show $ n-m)

nimMove :: NimGame -> IO String
nimMove gs@(Nim (_, piles)) = do
    let nimsum = foldr (xor) 0 piles
        moveset = [(n, m) | n <- piles, m <- [0..n-1]]
        goodmove = if nimsum == 0 then const True else (\(n, m) -> nimsum == xor n m)
    randomAction $ fmap movename $ filter goodmove moveset

nimGame :: [Integer] -> NimGame
nimGame x = Nim (Maximizer, x)

nimSolver :: NimGame -> StatelessSolvedGame
nimSolver = statelessSolver nimMove
