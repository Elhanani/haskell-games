{-# LANGUAGE ExistentialQuantification, NamedFieldPuns, RankNTypes, BangPatterns #-}

module Alphabeta (alphabetaSolver, chAlphabetaSolver) where

import SolverDefs
import Data.Maybe
import Data.List
import System.Random

-- Improvements:
-- 0. Consolidate alphabeta and chAlphabeta
-- 1. Memoizations
-- 2. Node heuristics

type ABValue = (Value, Value)

data ABParams gs = ABParams {alpha :: {-# UNPACK #-} !Value, beta :: {-# UNPACK #-} !Value,
                             chooser :: Maybe (ChoiceHeuristic gs)}

defaultABParams :: forall gs. (GameState gs) => ABParams gs
defaultABParams = ABParams {alpha = -1, beta = 1, chooser = Nothing}

type ChoiceHeuristic gs = gs -> String -> Value

type StateHeuristic gs = gs -> Value

data ABSolvedGame = forall gs. (GameState gs) =>
  ABSolvedGame {gameState :: gs,
                actions' :: [(String, ABSolvedGame)],
                abvalue :: ABValue -> Value,
                value :: Value}

instance Show ABSolvedGame where
  show (ABSolvedGame {gameState}) = show gameState

instance GameState ABSolvedGame where
  player (ABSolvedGame {gameState}) = player gameState
  terminal (ABSolvedGame {gameState}) = terminal gameState
  maxdepth (ABSolvedGame {gameState}) = maxdepth gameState
  actions = actions'

-- This may be multithreaded!
instance SolvedGameState ABSolvedGame where
  action g = do
    rand <- newStdGen
    return $ head $ filter ((== value g) . value . snd) $ fst $ shuffle (actions g) rand

shuffle :: forall a rg. (RandomGen rg) => [a] -> rg -> ([a], rg)
shuffle [] rand = ([], rand)
shuffle xs rand = (x:rest', rand'') where
  (k, rand') = randomR (0, length xs - 1) rand
  x = xs !! k
  rest = (take k xs) ++ (drop (k+1) xs)
  (rest', rand'') = shuffle rest rand'

alphabetaSolver :: (GameState a) => ABValue -> a -> ABSolvedGame
alphabetaSolver !ab !gameState = ABSolvedGame {gameState, actions', abvalue = abval, value} where
  internal (!str, !gs) = (str, alphabetaSolver ab gs)
  actions' = map internal $ actions gameState
  !tval = terminal gameState
  value = abval ab
  abval = if isJust $ tval then const $ fromJust tval else alphabetize actions' where
    alphabetize [(_, ns)] !ab' = abvalue ns ab'
    alphabetize !(x:xs) !(a, b) = let
      !kidval = abvalue (snd x) (a, b)
      in if a >= b then kidval else case player gameState of
        Maximizer -> max kidval $ alphabetize xs (max a kidval, b)
        Minimizer -> min kidval $ alphabetize xs (a, min b kidval)

chAlphabetaSolver :: (GameState a) => ChoiceHeuristic a -> ABValue -> a -> ABSolvedGame
chAlphabetaSolver !chooser !ab !gameState = ABSolvedGame {gameState, actions', abvalue = abval, value} where
  internal (!str, !gs) = (str, alphabetaSolver ab gs)
  actions' = map internal $ sortOn (chooser gameState . fst) $! actions gameState
  !tval = terminal gameState
  value = abval ab
  abval = if isJust $ tval then const $ fromJust tval else alphabetize actions' where
    alphabetize [(_, ns)] !ab' = abvalue ns ab'
    alphabetize !(x:xs) !(a, b) = let
      !kidval = abvalue (snd x) (a, b)
      in if a >= b then kidval else case player gameState of
        Maximizer -> max kidval $ alphabetize xs (max a kidval, b)
        Minimizer -> min kidval $ alphabetize xs (a, min b kidval)


-- memo should not be hard - we just figure out when we can be sure of the value
-- that happens when alpha and beta are intial, or when we get a "win" value
-- also when we improve an already existing bound

-- -- These should be multithreaded!
-- nhAlphabetaSolver :: (GameState a) => StateHeuristic a -> Int ->
--                                       ABValue -> a -> ABSolvedGame
-- nhAlphabetaSolver eval depth ab gs = undefined
--
-- nchAlphabetaSolver :: (GameState a) => ChoiceHeuristic a -> StateHeuristic a -> Int ->
--                                        ABValue -> a -> ABSolvedGame
-- nchAlphabetaSolver chooser eval depth ab gs = undefined


{- A simple example of a choice heuristic is to choose nodes with less actions.
   The assumption is that :
   1. Forcing an opponent is generally a good thing
   2. The subtree may be smaller and so quicker to traverse
-}
