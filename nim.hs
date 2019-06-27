import SolverDefs
import Data.List
import System.Environment
import Data.Bits
import System.Random
import qualified Data.Set as Set

newtype NimGame = Nim (Bool, [Integer]) deriving Eq

instance Show NimGame where
  show (Nim (first, piles)) = (unwords $ map show piles) ++ message where
    message = "\nIt's " ++ pl ++ " player's turn\n"
    pl = if first then "first" else "second"

instance GameState NimGame where
  firstplayer (Nim (first, _)) = first
  terminal (Nim (first, piles)) = if null piles then Just (if first then (-1) else 1) else Nothing
  actions (Nim (first, piles)) = [(movename (n, m), newng n m) | n <- uniq piles, m <- [0..n-1]] where
    uniq = Set.toList . Set.fromList
    newng n m = Nim (not first, filter (/= 0) $ a ++ [m] ++ b) where
      (a, b) = fmap (drop 1) $ break (n ==) piles

movename :: (Integer, Integer) -> String
movename (n, m) = (show n) ++ "-" ++ (show $ n-m)

nimSolver :: NimGame -> IO String
nimSolver gs@(Nim (_, piles)) = do
    let nimsum = foldr (xor) 0 piles
        moveset = [(n, m) | n <- piles, m <- [0..n-1]]
        goodmove = if nimsum == 0 then const True else (\(n, m) -> nimsum == xor n m)
    randomAction $ fmap movename $ filter goodmove moveset

pilemaker :: StdGen -> [String] -> [Integer]
pilemaker rand [] = pilemaker rand [show $ head $ (randomRs (3, 10) rand :: [Int])]
pilemaker rand [n] = pilemaker rand [n, show $ 10 + quot 100 (read n)]
pilemaker rand [n, h] = map toInteger $ take (read n) $ tail $ (randomRs (1, read h) rand :: [Int])
pilemaker _ strs = map read strs

main = do
  args <- getArgs
  rand <- newStdGen
  let initial = Nim (True, pilemaker rand args)
  putStrLn $ "\n\nWelcome to Nim!"
  putStrLn $ "Take any amount of stones from any pile, last player to move wins!"
  putStrLn $ "To take 'y' stones from a pile of size 'x', type 'x-y'\n\n"
  interaction initial humanSolver $ statelessSolver nimSolver