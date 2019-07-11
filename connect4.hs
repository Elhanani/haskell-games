{-# LANGUAGE BangPatterns, NamedFieldPuns #-}

import Data.List
import Data.Maybe
import SolverDefs
import Cache
import MCTS
import qualified Data.Array as A
import qualified PQueueQuick as PQ

data Square = Ex | Oh | None deriving Eq
data BoardState = Board {content :: A.Array (Int, Int) Square,
                         heights :: A.Array Int Int,
                         totalmoves :: Int,
                         terminal' :: Maybe Double,
                         numactions' :: Int} deriving Eq

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
    cols = (limitboard [(x-i, y) | i <- [1..3]], [])
    diag1 = (limitboard [(x-i, y-i) | i <- [1..3]], limitboard [(x+i, y+i) | i <- [1..3]])
    diag2 = (limitboard [(x+i, y-i) | i <- [1..3]], limitboard [(x-i, y+i) | i <- [1..3]])

-- | Will playing in that position complete a win?
isWinner :: BoardState -> Square -> Int -> Bool
isWinner (!Board {content, heights}) !player !col =
  or $ map f $! compsarr A.! pos where
    !pos = (heights A.! col, col)
    stretch = length . (takeWhile (\ix -> content A.! ix == player))
    f (!dir1, !dir2) = remaining2 == 0 where
      !remaining1 = 3 - stretch dir1
      !remaining2 = remaining1 - (stretch $ take remaining1 dir2)

-- | Next state after a move
mkState :: BoardState -> Int -> BoardState
mkState !gs@(Board {content, heights, totalmoves, numactions'}) !col =
  Board {content=con', heights=hei', totalmoves=tot', numactions'=num', terminal'=ter'} where
    (!sqrtype, !winval) = case mod totalmoves 2 of
      0 -> (Ex, Just 1)
      1 -> (Oh, Just (-1))
    !height = heights A.! col
    !draw = totalmoves == 41
    !tot' = totalmoves + 1
    !con' = content A.// [((height, col), sqrtype)]
    !hei' = heights A.// [(col, height+1)]
    !num' = if height == 5 then numactions'-1 else numactions'
    !ter' = if isWinner gs sqrtype col then winval
      else if draw then Just 0 else Nothing

-- | An empty board
initial :: BoardState
initial = Board {content = A.listArray ((0,0), (5, 6)) $ repeat None,
                 heights = A.listArray (0, 6) $ repeat 0,
                 totalmoves = 0,
                 terminal' = Nothing,
                 numactions' = 7}

-- main = do
--   let x = mtmctsSolver 3 defaultMCParams initial
--   print $ map simulations $ mtNodes x
--   f <- think x
--   getLine
--   y <- f
--   print $ map simulations $ mtNodes y

-- main = putStrLn "" >> (humanInteraction initial $ mtmctsSolver 3 $ defaultMCParams)

-- solver1 = mtmctsSolver 3 $ defaultMCParams {background = False, duration = 1000}
-- solver2 = mctsSolver $ defaultMCParams {background = False, duration = 1000}
solver3 = mctsSolver $ defaultMCParams
main = putStrLn "\n\n\n" >> interaction initial solver3 solver3

-- main = do
--   x <- multitimed initial 2500
--   print $ sum $ map snd x
