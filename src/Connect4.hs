{-# LANGUAGE BangPatterns, NamedFieldPuns #-}

module Connect4 (connect4game) where

import Data.List
import Data.Maybe
import SolverDefs
import Data.Hashable
import qualified Data.Array as A

data Square = Ex | Oh | None deriving Eq
data BoardState = Board {content :: A.Array (Int, Int) Square
                       , heights :: A.Array Int Int
                       , totalmoves :: Int
                       , terminal' :: Maybe Double
                       , numactions' :: Int
                       , identifier :: Integer}

instance Eq BoardState where
 (Board {identifier=id1}) == (Board {identifier=id2}) = id1 == id2

instance Ord BoardState where
 compare (Board {identifier=id1}) (Board {identifier=id2}) = compare id1 id2

instance Hashable BoardState where
 hashWithSalt n (Board {identifier}) = hashWithSalt n identifier

instance Show BoardState where
  show gs@(Board {content, terminal'}) = top ++ middle ++ bottom ++ message where
    top = "-------------\n"
    middle = unlines $ map row [0..5]
    bottom = "-------------\nA B C D E F G\n\n"
    message = if isNothing terminal' then playermessage ++ movemessage else case fromJust terminal' of
      1 -> "Mrs. Cross wins!"
      0 -> "It's a draw!"
      (-1) -> "Mr. Knott wins!"
    playermessage = case player gs of
      Maximizer -> "It's Mrs. Cross' turn.\n"
      Minimizer ->  "It's Mr. Knott's turn.\n"
    movemessage = "Possible moves are: " ++ (unwords $ map fst $ actions gs) ++ "\n"
    row n = intersperse ' ' [f $ content A.! (5-n, i) | i <- [0..6]]
    f Ex = 'X'
    f Oh = 'O'
    f None = ' '

movename :: Int -> String
movename n = ("abcdefg" !! n) : []

instance GameState BoardState where
  player Board {totalmoves} = if mod totalmoves 2 == 0 then Maximizer else Minimizer
  terminal = terminal'
  actions !gs@(Board {heights}) = [(movename n, mkState gs n) | n <- [0..6], (heights A.! n) < 6]
  numactions = numactions'
  maxdepth !gs = Just $! 9 - (totalmoves gs)

-- | The array corresponding to the required positions needed to complete a win
compsarr :: A.Array (Int, Int) [([(Int, Int)], [(Int, Int)])]
compsarr = A.array ((0,0), (5, 6)) $ map comps $ (,) <$> [0..5] <*> [0..6] where
  comps (x, y) = ((x, y), limit4 [rows, cols, diag1, diag2]) where
    limit4 = filter (\(a, b) -> length (a++b) >= 3)
    limitboard = filter (\(a, b) -> a >= 0 && a <= 5 && b >=0 && b <= 6)
    rows = (limitboard [(x, y-i) | i <- [1..3]], limitboard [(x, y+i) | i <- [1..3]])
    cols = (limitboard [(x-i, y) | i <- [1..3]], limitboard [(x+i, y) | i <- [1..3]])
    -- cols = (limitboard [(x-i, y) | i <- [1..3]], [])
    diag1 = (limitboard [(x-i, y-i) | i <- [1..3]], limitboard [(x+i, y+i) | i <- [1..3]])
    diag2 = (limitboard [(x+i, y-i) | i <- [1..3]], limitboard [(x-i, y+i) | i <- [1..3]])

-- | Quickly finding the identifier
idarr :: A.Array (Int, Int) Integer
idarr = A.array ((0,0), (5, 6)) $ (\x y -> ((x, y), 3^(x+7*y))) <$> [0..5] <*> [0..6] where

-- | Will playing in that position complete a win?
isWinner :: BoardState -> Square -> Int -> Bool
isWinner (!Board {content, heights}) !player !col =
  or $ map f $! compsarr A.! pos where
    !pos = (heights A.! col, col)
    stretch = length . (takeWhile (\ix -> content A.! ix == player))
    f (!dir1, !dir2) = remaining2 == 0 where
      !remaining1 = 3 - stretch dir1
      !remaining2 = remaining1 - (stretch $ take remaining1 dir2)

-- | Needs to be redesigned.
--   Phase 1 constructs the largest interval without X / without Y that are at most 3 squares
--   apart from the current square. Then it turns it into a binary number, and get it score
--   the scores would be computed in advance. We can ask what is the chance of X winning if
--   all of the empties are random and the same for Y, assuming that the specific square gets X/Y.
moveHeuristic :: Int -> Int -> BoardState -> Player -> Int -> Int
moveHeuristic !a !b s@(!Board {content, heights}) !player !col =
  bonus + (sum $ map g $! compsarr A.! pos) where
    !pos = (heights A.! col, col)
    stretch !p = length . (takeWhile (\ix -> content A.! ix /= p))
    f (!p, !c) (!dir1, !dir2) = c * (max 0 (squares-2)) where
      !squares = (stretch p dir1) + (stretch p dir2)
    g !dirs = case player of
      Maximizer -> (f (Ex, b) dirs) + (f (Oh, a) dirs)
      Minimizer -> (f (Oh, b) dirs) + (f (Ex, a) dirs)
    bonus = case player of
      Maximizer -> 100*(a+b)*(10*(fromEnum $ isWinner s Ex col) + (fromEnum $ isWinner s Oh col))
      Minimizer -> 100*(a+b)*(10*(fromEnum $ isWinner s Oh col) + (fromEnum $ isWinner s Ex col))

-- | Next state after a move
mkState :: BoardState -> Int -> BoardState
mkState !gs@(Board {content, heights, totalmoves, numactions', identifier}) !col =
  Board {content=con', heights=hei', totalmoves=tot',
         numactions'=num', terminal'=ter', identifier=id'} where
    (!sqrtype, !winval, !id') = case mod totalmoves 2 of
      0 -> (Ex, Just 1, identifier - idarr A.! (height, col))
      1 -> (Oh, Just (-1), identifier + idarr A.! (height, col))
    !height = heights A.! col
    !draw = totalmoves == 41
    !tot' = totalmoves + 1
    !con' = content A.// [((height, col), sqrtype)]
    !hei' = heights A.// [(col, height+1)]
    !num' = if height == 5 then numactions'-1 else numactions'
    !ter' = if isWinner gs sqrtype col then winval
      else if draw then Just 0 else Nothing

-- | An empty board
connect4game :: BoardState
connect4game = Board {content = A.listArray ((0,0), (5, 6)) $ repeat None
                    , heights = A.listArray (0, 6) $ repeat 0
                    , totalmoves = 0
                    , terminal' = Nothing
                    , numactions' = 7
                    , identifier = 0}
