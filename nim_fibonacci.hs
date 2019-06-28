{-# LANGUAGE NamedFieldPuns #-}

import SolverDefs
import Data.List
import Data.Ratio
import System.Environment
import System.Random

data NimGame = Nim {player' :: Player,
                    limit :: Integer,
                    pile :: Integer} deriving Eq

instance Show NimGame where
  show (Nim {player' = Maximizer, pile = 0}) = "Second player has no more moves.\nFirst player wins!"
  show (Nim {player' = Minimizer, pile = 0}) = "First player has no more moves.\nSecond player wins!"
  show (Nim {limit, pile, player'}) = pilestr ++ playerstr ++ movestr where
    pilestr = "There are " ++ (show pile) ++ " stones in the pile.\n"
    movestr = if limit < pile
      then "You can take at most " ++ (show limit) ++ " of them...\n"
      else "You can take as many of them as you'd like!\n"
    playerstr = case player' of
      Maximizer -> "First player's turn to make a move.\n"
      Minimizer -> "Second player's turn to make a move.\n"

instance GameState NimGame where
  player = player'
  terminal (Nim {player', pile = 0}) = Just $ playerValue player'
  terminal _ = Nothing
  actions (Nim {player', pile, limit}) = [(show n, newnim n) | n <- [1..limit]] where
    newnim n = Nim {player' = otherPlayer player', pile = pile-n, limit = min (pile-n) (2*n)}

fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

zeckendorf :: Integer -> [Integer]
zeckendorf n = internal n fibs where
  fibs = reverse $ takeWhile (<= n) fibonacci
  internal 0 _ = []
  internal k (x:xs)
    | k < x = internal k xs
    | otherwise  = x : internal (k-x) xs

-- | The guaranteed solution
nimsolver1 :: NimGame -> String
nimsolver1 (Nim {pile = p, limit = l})
  | l >= p = show p
  | l >= r = show r
  | otherwise = "1"
  where r = last $ zeckendorf p

-- | The quickest solution for a win
nimsolver2 :: NimGame -> String
nimsolver2 (Nim {pile = p, limit = l}) = if null wins then "1" else show $ head wins where
  q = min l $ quot (p-1) 3
  trivial = if l < p then [] else [p]
  wins = trivial ++ filter isgood [q,q-1..1]
  isgood n = (last $ zeckendorf $ p-n) > 2*n

initial :: Integer -> NimGame
initial p = Nim {player' = Maximizer, limit = p-1, pile = p}

-- | Just sets random parameters for the initial state
boundary :: [Integer] -> [Integer]
boundary [] = boundary [100]
boundary [n] = boundary [n, quot n 2]
boundary xs = [minimum xs, maximum xs]

main = do
  args <- getArgs
  rand <- newStdGen
  let [b1, b2] = boundary $ map read $ args
      p = head $ randomRs (b1, b2) rand
  putStrLn $ "Welcome to Fibonacci Nim!"
  putStrLn $ "On the first turn you can take as much stones as you wish (excpet all of them)"
  putStrLn $ "On each subsequent turn, you are allowed to take at most twice the amount taken the previous turn\n\n"
  interaction (initial p) humanSolver $ statelessSolver (return . nimsolver2)
