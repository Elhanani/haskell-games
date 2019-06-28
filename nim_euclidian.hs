{-# LANGUAGE NamedFieldPuns #-}

import SolverDefs
import Data.List
import Data.Ratio
import Control.Monad.State
import System.Environment
import System.Random


-- | The message is an artifact of a previous computation.
data NimGame = Nim {player' :: Player,
                    message :: String,
                    p1 :: Integer,
                    p2 :: Integer}

-- | This produces correct results from the game's perspective, but may show the wrong thing.
instance Eq NimGame where
  Nim {player'=f1, p1=p11, p2=p12} == Nim {player'=f2, p1=p21, p2=p22} = (f1, p11, p12) == (f2, p21, p22)

gameOver :: Integer -> Player -> String
gameOver n player' = "Only one pile left: " ++ (show n) ++ movestr ++ winstr ++ "\n" where
  movestr = "\nNo more moves for " ++ playerstr player' ++ ", "
  winstr = playerstr player' ++ " wins!"
  playerstr Maximizer = "second"
  playerstr Minimizer = "first"

instance Show NimGame where
  show (Nim {player', p1 = 0, p2}) = gameOver p2 player'
  show (Nim {player', p1, p2 = 0}) = gameOver p1 player'
  show (Nim {message, p1, p2}) = message ++  pilestr ++ movestr where
    pilestr = "The piles on the table are " ++ (show p1) ++ " and " ++ (show p2) ++ "\n"
    [a, b] = sort [p1, p2]
    maxmove = quot b a
    movestr = if maxmove == 1 then "The only possible move is 1\n" else
      "Possible moves are between 1 and " ++ (show maxmove) ++ " (inclusive)\n"

-- | The game's solution
nimsolver :: NimGame -> String
nimsolver Nim {p1, p2} = show play where
  [small, big] = sort [p1, p2]
  q = quot big small
  m = mod big small
  ratio1 = small % m
  ratio2 = 1 + m % small
  play = if q == 1 then 1 else if m == 0 then q else if ratio1 < ratio2 then q else q-1

instance GameState NimGame where
  player = player'
  terminal (Nim {player', p1 = 0}) = Just $ playerValue player'
  terminal (Nim {player', p2 = 0}) = Just $ playerValue player'
  terminal _ = Nothing
  actions (Nim {player', p1, p2})
    | p1 > p2 = [(show n, newng (p1,n,p2) (p1-n*p2) p2) | n <- [1..quot p1 p2]]
    | otherwise = [(show n, newng (p2,n,p1) p1 (p2-n*p1)) | n <- [1..quot p2 p1]]
    where
      newng tup p1 p2 = Nim {player' = otherPlayer player', message = msg tup, p1, p2}
      msg (big, q, small) = (show big) ++ " - " ++ (show q) ++ "*" ++ (show small) ++
                             " = " ++ (show $ big-q*small) ++ "\n"

initial :: Integer -> Integer -> NimGame
initial p1 p2 = Nim {player' = Maximizer, message = "", p1, p2}

-- | Just sets random parameters for the initial state
boundary :: [Integer] -> [Integer]
boundary [] = boundary [10000]
boundary [n] = boundary [n, quot n 10]
boundary xs = [minimum xs, maximum xs]

main = do
  args <- getArgs
  rand <- newStdGen
  let [b1, b2] = boundary $ map read $ args
      (p1:p2:_) = take 2 $ randomRs (b1, b2) rand
  putStrLn $ "Welcome to Euclidian Nim!"
  putStrLn $ "Legal moves are taking integer multiples of the small pile out of the big pile\n\n"
  interaction (initial p1 p2) humanSolver $ statelessSolver (return . nimsolver)
