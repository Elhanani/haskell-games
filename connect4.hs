{-# LANGUAGE BangPatterns, NamedFieldPuns #-}

import Data.List
import Data.Maybe
import SolverDefs
import MCTS
import qualified Data.Array as A
import qualified PQueueQuick as PQ
import System.Random

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
    playermessage = if firstplayer gs then "It's Mrs. Cross' turn.\n" else "It's Mr. Knott's turn.\n"
    movemessage = "Possible moves are: " ++ (unwords $ map fst $ actions gs) ++ "\n"
    row n = intersperse ' ' [f $ content A.! (5-n, i) | i <- [0..6]]
    f Ex = 'X'
    f Oh = 'O'
    f None = ' '

movename :: Int -> String
movename n = ("abcdefg" !! n) : []

instance GameState BoardState where
  firstplayer = (== 0) . (flip mod 2) . totalmoves
  terminal = terminal'
  actions !gs@(Board {heights}) = [(movename n, mkState gs n) | n <- [0..6], (heights A.! n) < 6]
  numactions = numactions'
  maxdepth !gs = Just $! 9 - (totalmoves gs)

compsarr :: A.Array (Int, Int) [([(Int, Int)], [(Int, Int)])]
compsarr = A.array ((0,0), (5, 6)) $ map comps $ (,) <$> [0..5] <*> [0..6] where
  comps (x, y) = ((x, y), limit4 [rows, cols, diag1, diag2]) where
    limit4 = filter (\(a, b) -> length (a++b) >= 3)
    limitboard = filter (\(a, b) -> a >= 0 && a <= 5 && b >=0 && b <= 6)
    rows = (limitboard [(x, y-i) | i <- [1..3]], limitboard [(x, y+i) | i <- [1..3]])
    cols = (limitboard [(x-i, y) | i <- [1..3]], [])
    diag1 = (limitboard [(x-i, y-i) | i <- [1..3]], limitboard [(x+i, y+i) | i <- [1..3]])
    diag2 = (limitboard [(x+i, y-i) | i <- [1..3]], limitboard [(x-i, y+i) | i <- [1..3]])

isWinner :: BoardState -> Square -> Int -> Bool
isWinner (!Board {content, heights}) !player !col =
  or $ map f $! compsarr A.! pos where
    !pos = (heights A.! col, col)
    stretch = length . (takeWhile (\ix -> content A.! ix == player))
    f (!dir1, !dir2) = remaining2 == 0 where
      !remaining1 = 3 - stretch dir1
      !remaining2 = remaining1 - (stretch $ take remaining1 dir2)


mkState :: BoardState -> Int -> BoardState
mkState !gs@(Board {content, heights, totalmoves, numactions'}) !col =
  Board {content=con', heights=hei', totalmoves=tot', numactions'=num', terminal'=ter'} where
    !first = firstplayer gs
    !sqrtype = if first then Ex else Oh
    !height = heights A.! col
    !winval = Just $! if first then 1 else -1
    !draw = totalmoves == 41
    !tot' = totalmoves + 1
    !con' = content A.// [((height, col), sqrtype)]
    !hei' = heights A.// [(col, height+1)]
    !num' = if height == 5 then numactions'-1 else numactions'
    !ter' = if isWinner gs sqrtype col then winval
      else if draw then Just 0 else Nothing

initial :: BoardState
initial = Board {content = A.listArray ((0,0), (5, 6)) $ repeat None,
                 heights = A.listArray (0, 6) $ repeat 0,
                 totalmoves = 0,
                 terminal' = Nothing,
                 numactions' = 7}

main = do
  rand <- newStdGen
  putStrLn ""
  let mymctssolver = mctsSolver (MCParams {alpha=(-1),
                                           beta=1,
                                           evalfunc=ucb1 2,
                                           duration=500,
                                           simulations = 1000000,
                                           defrand=rand})
  -- interaction initial mymctssolver mymctssolver
  humanInteraction initial mymctssolver

-- main = do
--   x <- multitimed initial 2500
--   print $ sum $ map snd x
