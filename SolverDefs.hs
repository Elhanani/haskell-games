{-# LANGUAGE ExistentialQuantification
           , RankNTypes
           , NamedFieldPuns
           , BangPatterns
             #-}

module SolverDefs (Value, GameState, SolvedGameState,
                   firstplayer, terminal, actions, numactions, maxdepth, act,
                   showmoves, showhelp, action,
                   randomAction, statelessSolver, randomSolver, humanSolver,
                   interaction) where

import System.Random
import System.IO
import Data.Maybe

type Value = Double

class (Show gs) => GameState gs where
  firstplayer :: gs -> Bool
  terminal :: gs -> Maybe Value
  actions :: gs -> [(String, gs)]
  numactions :: gs -> Int
  maxdepth :: gs -> Maybe Int
  act :: gs -> String -> Maybe gs
  showmoves :: gs -> String
  showhelp :: gs -> String

  numactions g = length $! actions g
  maxdepth = const Nothing
  act g str = lookup str $ actions g
  showmoves g = "Possible moves are: " ++ (unwords $ map fst $ actions g)
  showhelp = showmoves

class (GameState gs) => SolvedGameState gs where
  action :: gs -> IO (String, gs)

data StatelessSolvedGame = forall gs. GameState gs =>
  StatelessSolvedGame {gameState :: gs,
                       action' :: IO String,
                       actions' :: [(String, StatelessSolvedGame)]}

instance Show StatelessSolvedGame where
  show (StatelessSolvedGame {gameState}) = show gameState

instance GameState StatelessSolvedGame where
  firstplayer (StatelessSolvedGame {gameState}) = firstplayer gameState
  terminal (StatelessSolvedGame {gameState}) = terminal gameState
  actions = actions'

instance SolvedGameState StatelessSolvedGame where
  action gs = do
    move <- action' gs
    return (move, fromJust $ act gs move)

randomAction :: forall a. [a] -> IO a
randomAction !xs = do
  rand <- newStdGen
  return $ xs !! (fst $! randomR (0, (length xs - 1)) rand)

statelessSolver :: (GameState a) => (a -> IO String) -> a -> StatelessSolvedGame
statelessSolver f gameState = StatelessSolvedGame {gameState, actions', action'} where
  internal (str, ng) = (str, statelessSolver f ng)
  actions' = map internal $ actions gameState
  action' = f gameState

randomSolver :: (GameState a) => a -> StatelessSolvedGame
randomSolver = statelessSolver (fmap fst . randomAction . actions)

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
        then (putStrLn $ move ++ " is not a valid move!\n Enter 'moves' to list possible moves.\n") >> humanaction gs
        else return move

interaction :: forall a b c. (GameState a, SolvedGameState b, SolvedGameState c) =>
                               a -> (a -> b) -> (a -> c) -> IO Value
interaction gs f1 f2 = interaction' (f1 gs) (f2 gs)

interaction' :: forall a b. (SolvedGameState a, SolvedGameState b) => a -> b -> IO Value
interaction' s1 s2 = if isJust $ terminal s1
  then print s1 >> putStrLn "\n" >> (return $ fromJust $ terminal s1)
  else do
    print s1
    (move, ns1) <- action s1
    putStrLn $ "Move chosen: " ++ move ++ "\n\n"
    let Just ns2 = act s2 move
    interaction' ns2 ns1
