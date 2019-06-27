import SolverDefs
import Minmax
import Alphabeta
import Data.List.Split
import Data.Maybe
import qualified Data.Array as A

data Square = Ex | Oh | None deriving (Eq, Ord)
type Miniboard = A.Array Int Square

newtype BoardState = Board (Int, Miniboard, Maybe Value) deriving (Eq, Ord)

instance Show BoardState where
  show gs@(Board (n, board, v)) = showboard ++ "\n" ++ message where
    showboard = unlines $ (zipWith (++) legend) $ chunksOf 3 $ map f $ A.elems board where
    f Ex = 'X'
    f Oh = 'O'
    f None = '.'
    legend = ["123 ", "456 ", "789 "]
    message = if isNothing v then playermessage ++ movemessage else case fromJust v of
      1 -> "Mrs. Cross wins!"
      0 -> "It's a draw!"
      (-1) -> "Mr. Knott wins!"
    playermessage = if firstplayer gs then "It's Mrs. Cross' turn.\n" else "It's Mr. Knott's turn.\n"
    movemessage = "Possible moves are " ++ (unwords $ map fst $ actions gs) ++ "\n"

instance GameState BoardState where
  firstplayer (Board (n, _, _)) = mod n 2 == 0
  terminal (Board (_, _, v)) = v
  actions gs@(Board (_, _, v)) = catMaybes [fmap ((,) (show (n+1))) (mkState gs n) | n <- [0..8]]

winners :: [[Int]]
winners = [[0, 1, 2], [3, 4, 5], [6, 7, 8],
           [0, 3, 6], [1, 4, 7], [2, 5, 8],
           [0, 4, 8], [2, 4, 6]]

complements :: Int -> [[Int]]
complements = (arr A.!) where
  arr = A.listArray (0, 8) $ map comps [0..8]
  comps n = map (filter (/= n)) $ filter (elem n) winners

isWinner :: Square -> Int -> BoardState -> Bool
isWinner player pos (Board (_, xs, _)) = or [and $ map f comps | comps <- complements pos]
  where f n = xs A.! n == player

mkState :: BoardState -> Int -> Maybe BoardState
mkState gs@(Board (l, b, v)) n = if b A.! n /= None then Nothing else let
  first = firstplayer gs
  sqrtype = if first then Ex else Oh
  winval = Just $ if first then 1 else -1
  nb = b A.// [(n, sqrtype)]
  winner = isWinner sqrtype n gs
  next x = Just $ Board (l+1, nb, x)
  in if winner then next $ winval else if l == 8 then next (Just 0) else next Nothing

initial :: BoardState
initial = Board (0, A.listArray (0, 8) $ repeat None, Nothing)

main = putStrLn "\n" >> interaction initial absolver absolver where
  absolver = alphabetaSolver (-1, 1)
