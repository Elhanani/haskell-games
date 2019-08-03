{-# LANGUAGE ExistentialQuantification
           , RankNTypes
           , NamedFieldPuns
           , BangPatterns
             #-}

module SolverDefs (Value, Player(..), GameState(..), SolvedGameState(..),
                   playerValue, playerObjective, otherPlayer, randomAction,
                   StatelessSolvedGame, statelessSolver, randomSolver, humanSolver,
                   interaction, humanInteraction) where

import System.Random
import System.IO
import Data.Maybe

type Value = Double
data Player = Maximizer | Minimizer deriving (Eq, Ord)

playerValue :: Player -> Value
playerValue Maximizer = 1
playerValue Minimizer = -1

playerObjective :: (Foldable t, Ord a) => Player -> t a -> a
playerObjective Maximizer = maximum
playerObjective Minimizer = minimum

otherPlayer :: Player -> Player
otherPlayer Maximizer = Minimizer
otherPlayer Minimizer = Maximizer

-- | A GameState is a representation of a specific state of a game.
class (Show gs) => GameState gs where
  -- | Which player's turn is it to play in the given node
  player :: gs -> Player
  -- | Just the value of a terminal node, or Nothing if the node is non terminal
  terminal :: gs -> Maybe Value
  -- | Branches leaving the game state. Each element in the list is made
  --   of the move's name and the new state it leads to.
  actions :: gs -> [(String, gs)]
  -- | Number of actions available. Implement specificly for better performance.
  numactions :: gs -> Int
  -- | Maximal number of moves until the game finishes, or Nothing if unknown.
  maxdepth :: gs -> Maybe Int
  -- | Perform the move to get a new game state
  act :: gs -> String -> Maybe gs
  -- | The message to show when a user inputs "moves"
  showmoves :: gs -> String
  -- | The message to show when a user inputs "help"
  showhelp :: gs -> String

  numactions g = length $! actions g
  maxdepth = const Nothing
  act g str = lookup str $ actions g
  showmoves g = "Possible moves are: " ++ (unwords $ map fst $ actions g)
  showhelp = showmoves

-- | A solved game state is a superclass of a game
class (GameState gs) => SolvedGameState gs where
  -- | Best action to play according to the solver, for the given state.
  action :: gs -> IO (String, gs)
  -- | Perform computation on another thread
  think :: gs -> IO (IO gs)

  think g = return $ return g

-- | A generic way to define a game solver
data StatelessSolvedGame = forall gs. GameState gs =>
  StatelessSolvedGame {gameState :: gs,
                       action' :: IO String,
                       actions' :: [(String, StatelessSolvedGame)]}

instance Show StatelessSolvedGame where
  show (StatelessSolvedGame {gameState}) = show gameState

instance GameState StatelessSolvedGame where
  player (StatelessSolvedGame {gameState}) = player gameState
  terminal (StatelessSolvedGame {gameState}) = terminal gameState
  actions = actions'

instance SolvedGameState StatelessSolvedGame where
  action gs = do
    move <- action' gs
    return (move, fromJust $ act gs move)

-- | Pick a random element from the given list
randomAction :: forall a. [a] -> IO a
randomAction !xs = do
  rand <- newStdGen
  return $ xs !! (fst $! randomR (0, (length xs - 1)) rand)

-- | Make a solver from a (game specific) functions
statelessSolver :: (GameState a) => (a -> IO String) -> a -> StatelessSolvedGame
statelessSolver f gameState = StatelessSolvedGame {gameState, actions', action'} where
  internal (str, ng) = (str, statelessSolver f ng)
  actions' = map internal $ actions gameState
  action' = f gameState

-- | Just plays randomly
randomSolver :: (GameState a) => a -> StatelessSolvedGame
randomSolver = statelessSolver (fmap fst . randomAction . actions)

-- | A human is a solver as well
humanSolver :: (GameState a) => a -> StatelessSolvedGame
humanSolver = statelessSolver humanaction where
  humanaction gs = do
    putStr "Please choose your move: "
    hFlush stdout
    move <- getLine
    case move of
      "quit" -> error "Game ended prematurely" -- prompt for confirmation
      "moves" -> (putStrLn $ showmoves gs ++ "\n") >> humanaction gs
      "help" -> (putStrLn $ showhelp gs ++ "\n") >> humanaction gs
      otherwise -> let ngs = act gs move in if isNothing ngs
        then (putStrLn $ move ++ " is not a valid move!\nEnter 'moves' to list possible moves.\n") >> humanaction gs
        else return move

-- | Make 2 game solvers to play together from a given initial state
interaction :: forall a b c. (GameState a, SolvedGameState b, SolvedGameState c) =>
                               a -> (a -> b) -> (a -> c) -> IO Value
interaction gs f1 f2 = interaction' (f1 gs) (f2 gs)

-- | Make 2 solved games play together
interaction' :: forall a b. (SolvedGameState a, SolvedGameState b) => a -> b -> IO Value
interaction' s1 s2 = if isJust $ terminal s1
  then print s1 >> putStrLn "\n" >> (return $ fromJust $ terminal s1)
  else do
    ts2 <- think s2
    print s1
    (move, ns1) <- action s1
    putStrLn $ "Move chosen: " ++ move ++ "\n\n"
    s2' <- ts2
    let Just ns2 = act s2' move
    interaction' ns2 ns1

-- | Make a solver play agains a human from an initial position
humanInteraction :: forall a b c. (GameState a, SolvedGameState b) => a -> (a -> b) -> IO Value
humanInteraction game solver = do
  first <- multiplechoice "Would you like to be the first or second player? " ["first", "second"]
  putStrLn "\n"
  case first of
    "first" -> interaction game humanSolver solver
    "second" -> interaction game solver humanSolver

-- | Poll a human from a set of possible responses
multiplechoice :: String -> [String] -> IO String
multiplechoice msg list = do
  putStr $ msg
  hFlush stdout
  ans <- getLine
  if elem ans list
    then return ans
    else (putStrLn $ "\n" ++ ans ++ " is not a valid answer.\n") >> multiplechoice msg list
