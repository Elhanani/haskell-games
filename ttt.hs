import SolverDefs
import Minmax
import Alphabeta
import Data.List.Split
import Data.Maybe
import Data.Hashable
import qualified Data.Array as A


data Square = Ex | Oh | None deriving (Eq, Ord, Enum)
type Miniboard = A.Array Int Square


instance Hashable Square where
  hashWithSalt n = (hashWithSalt n) . fromEnum

instance Hashable BoardState where
  hashWithSalt n (Board (_, _, h, _)) = hashWithSalt n h


-- | Number of moves played so far, the board itself, and the terminal value
newtype BoardState = Board (Int, Miniboard, Integer, Maybe Value) deriving (Eq, Ord)

instance Show BoardState where
  show gs@(Board (n, board, _, v)) = showboard ++ "\n" ++ message where
    showboard = unlines $ (zipWith (++) legend) $ chunksOf 3 $ map f $ A.elems board where
    f Ex = 'X'
    f Oh = 'O'
    f None = '.'
    legend = ["123 ", "456 ", "789 "]
    message = if isNothing v then playermessage ++ movemessage else case fromJust v of
      1 -> "Mrs. Cross wins!"
      0 -> "It's a draw!"
      (-1) -> "Mr. Knott wins!"
    playermessage = case player gs of
      Maximizer -> "It's Mrs. Cross' turn.\n"
      Minimizer ->  "It's Mr. Knott's turn.\n"
    movemessage = "Possible moves are " ++ (unwords $ map fst $ actions gs) ++ "\n"

instance GameState BoardState where
  player (Board (n, _, _, _)) = if mod n 2 == 0 then Maximizer else Minimizer
  terminal (Board (_, _, _, v)) = v
  actions gs@(Board (_, _, _, v)) = catMaybes [fmap ((,) (show (n+1))) (mkState gs n) | n <- [0..8]]

-- | The winning positions
winners :: [[Int]]
winners = [[0, 1, 2], [3, 4, 5], [6, 7, 8],
           [0, 3, 6], [1, 4, 7], [2, 5, 8],
           [0, 4, 8], [2, 4, 6]]

-- | Required extra 2 positions for a win
complements :: Int -> [[Int]]
complements = (arr A.!) where
  arr = A.listArray (0, 8) $ map comps [0..8]
  comps n = map (filter (/= n)) $ filter (elem n) winners

hasharr :: A.Array Int Integer
hasharr = A.listArray (0, 8) $ map (\x -> 3^x) [0..8]

-- | Is playing in this square wins the game?
isWinner :: Square -> Int -> BoardState -> Bool
isWinner player pos (Board (_, xs, _, _)) = or [and $ map f comps | comps <- complements pos]
  where f n = xs A.! n == player

-- | Produces the branch states
mkState :: BoardState -> Int -> Maybe BoardState
mkState gs@(Board (l, b, h, v)) n = if b A.! n /= None then Nothing else let
  player' = player gs
  sqrtype = case player' of
    Maximizer -> Ex
    Minimizer -> Oh
  winval = Just $ playerValue player'
  nb = b A.// [(n, sqrtype)]
  winner = isWinner sqrtype n gs
  z = hasharr A.! n
  nh = case player' of
    Maximizer -> h + z
    Minimizer -> h - z
  next x = Just $ Board (l+1, nb, nh, x)
  in if winner then next $ winval else if l == 8 then next (Just 0) else next Nothing

-- | Initial state
initial :: BoardState
initial = Board (0, A.listArray (0, 8) $ repeat None, 0, Nothing)

main = putStrLn "\n" >> interaction initial absolver absolver where
  -- absolver = hashMinmaxSolver --  alphabetaSolver (-1, 1)
  absolver = singleLRUMinmaxSolver 10000 --  alphabetaSolver (-1, 1)
