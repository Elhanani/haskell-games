{-# LANGUAGE BangPatterns #-}

module UTT (ultimateTTT) where

import SolverDefs
import Data.List
import Data.Maybe
import Data.List.Split
import Data.Either
import qualified Data.Array as A

-- Translate into uarray of ints in trinary representation
-- Can store 3 miniboards in one integer
data Square = Ex | Oh | None deriving (Eq, Ord)
type Miniboard = A.Array Int Square
type MiniPlus = Either Square (Int, Miniboard)
type Bigboard = A.Array Int MiniPlus

-- | (player, board state, next miniboard to play, terminal value)
newtype BoardState = Board (Player, Bigboard, Maybe Int, Maybe Value) deriving (Eq, Ord)

miniShow :: Bool -> MiniPlus -> [String]
miniShow _ (Left Ex) = ["\\ /", " X ", "/ \\"]
miniShow _ (Left Oh) = ["/^\\", "( )", "\\_/"]
miniShow _ (Left None) = ["   ", " % ", "   "]
miniShow playable (Right (_, mini)) = chunksOf 3 $ map internal $ A.elems mini where
  internal Ex = 'X'
  internal Oh = 'O'
  internal None = if playable then '.' else ' '

instance Show BoardState where
  show gs@(Board (_, minis, next, v)) = showboard ++ "\n" ++ message where
    showboard = legendify $ unlines $ intersperse seperator $ bigrows
    seperator = "----+-----+----"
    isplayable n = (next == Nothing) || (next == Just n)
    playable = map isplayable [0..8]
    ministr = zipWith miniShow playable $ A.elems minis
    bigrows = map row [0, 3, 6]
    row n = init $ unlines $ map (concat . (intersperse " | ")) $ transpose $ take 3 $ drop n $ ministr
    legend = "   ABC   DEF   GHI   \n"
    enclose [] a = a
    enclose (x:xs) a = x ++ (enclose xs a) ++ x
    legendify = enclose ["\n", legend] . unlines .
                zipWith (\x -> enclose [" ", [x], " "]) "123 456 789" . lines
    message = if isNothing v then playermessage ++ movemessage else case fromJust v of
      1 -> "Mrs. Cross wins!"
      0 -> "It's a draw!"
      (-1) -> "Mr. Knott wins!"
    playermessage = case player gs of
      Maximizer -> "It's Mrs. Cross' turn.\n"
      Minimizer -> "It's Mr. Knot's turn.\n"
    movemessage = "Possible moves are " ++ (unwords $ map fst $ actions gs) ++ "\n"

movename :: (Int, Int) -> String
movename !(a, b) = [x, y] where
  x = ("abcdefghi" !!) $ 3*(mod a 3) + (mod b 3)
  y = ("123456789" !!) $ 3*(quot a 3) + (quot b 3)

instance GameState BoardState where
  player !(Board (!player', _, _, _)) = player'
  terminal !(Board (_, _, _, !v)) = v
  actions !gs@(Board (_, _, Just !pos, _)) = miniActions gs $! pos
  actions !gs@(Board (_, !bb, Nothing, _)) = concatMap (miniActions gs) [0..8]
  numactions (Board (_, !bb, Just !pos, _)) = miniMovesNum $! bb A.! pos
  numactions (Board (_, !bb, Nothing, _)) = sum $! map (miniMovesNum . ((A.!) bb)) [0..8]
  maxdepth (Board (_, !bb, _, _)) = Just $! sum $! map (miniMovesNum . ((A.!) bb)) [0..8]

-- | Winning configurations for a miniboard
winners :: [[Int]]
winners = [[0, 1, 2], [3, 4, 5], [6, 7, 8],
           [0, 3, 6], [1, 4, 7], [2, 5, 8],
           [0, 4, 8], [2, 4, 6]]

-- | Required squares to complete a win
compsarr :: A.Array Int [[Int]]
compsarr = A.listArray (0, 8) $ map comps [0..8] where
  comps !n = map (filter (/= n)) $ filter (elem n) winners

-- | Will a move win a game
isMiniWinner :: Square -> Int -> Miniboard -> Bool
isMiniWinner !player !pos !xs = or [and $ map f comps | comps <- compsarr A.! pos]
  where f !n = xs A.! n == player

-- | How many moves in the miniboard
miniMovesNum :: MiniPlus -> Int
miniMovesNum !(Left _) = 0
miniMovesNum !(Right !(!n, !mini)) = 9 - n

-- | The possible moves of the miniboard
miniMoves :: MiniPlus -> [Int]
miniMoves !(Left _) = []
miniMoves !(Right !(_, !mini)) = [n | n <- [0..8], mini A.! n == None]

-- | Actions corresponding to the miniboard
miniActions :: BoardState -> Int -> [(String, BoardState)]
miniActions !gs@(Board (_, !bb, _, _)) x =
  map internal $! miniMoves $! bb A.! x where
    internal !y = (movename (x, y), mkState gs x y)

-- | Play the y'th square on the x'th miniboard
mkState :: BoardState -> Int -> Int -> BoardState
mkState !gs@(Board (!player', !bb, _, _)) !x !y = Board (otherPlayer player', nbb, nextpos, terminal) where
    !(!cnt, !rbbx) = fromRight undefined $! bb A.! x
    !sqrtype = case player' of
      Maximizer -> Ex
      Minimizer -> Oh
    !winner = cnt > 2 && isMiniWinner sqrtype y rbbx
    !draw = cnt == 8
    !nmb = if winner then Left sqrtype else
      if draw then Left None else Right (cnt+1, rbbx A.// [(y, sqrtype)])
    !nbb = bb A.// [(x, nmb)]
    !nextpos = if isLeft (nbb A.! y) then Nothing else Just y
    bigwinner = isMiniWinner sqrtype x $ fmap (fromLeft None) bb
    bigdraw = all isLeft $ A.elems nbb
    !terminal = if winner then
      if bigwinner
        then Just $! playerValue $! player'
        else if bigdraw then Just 0 else Nothing
      else if draw && bigdraw then Just 0 else Nothing

ultimateTTT :: BoardState
ultimateTTT = Board (Maximizer, array8 $ Right $ (0, array8 None), Nothing, Nothing) where
  array8 x = A.listArray (0, 8) $ repeat x

-- | Symmetries for the first move
symmetries :: [[String]]
symmetries = [["e5"], ["d4", "f4", "d6", "f6"], ["e4", "d5", "f5", "e6"],
              ["b2", "h2", "b8", "h8"], ["a1", "i1", "a9", "i9"], ["c3", "g3", "g7", "c7"],
              ["c1", "a3", "g1", "i3", "a7", "c9", "i7", "g9"],
              ["b1", "a2", "h1", "i2", "a8", "b9", "h9", "i8"],
              ["c2", "b3", "g2", "h3", "b7", "c8", "g8", "h7"],
              ["e2", "b5", "h5", "e8"], ["e1", "a5", "i5", "e9"], ["e3", "c5", "g5", "e7"],
              ["d1", "f1", "a4", "a6", "i4", "i6", "d9", "f9"],
              ["d2", "f2", "b4", "b6", "h4", "h6", "d8", "f8"],
              ["d3", "f3", "c4", "c6", "g4", "g6", "d7", "f7"]]
