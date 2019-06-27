module Interaction where

import SolverDefs
import System.IO
import System.Random


showGameState :: (Show a) => Bool -> Bool -> GameState (a, b) -> String
showGameState showmoves first ((a, b), rgs) = show a ++ (turn rgs) ++ moves where
  name = if not first then "First player's turn\n" else "Second player's turn\n"
  turn (Internal _) = name
  turn (Terminal _) = ""
  showRgs (Terminal x) = "Final position, value is " ++ show x
  showRgs (Internal choices) = "Moves: " ++ (unwords $ map fst choices)
  moves = if showmoves then (showRgs rgs) ++ "\n" else ""


yesno :: String -> IO Bool
yesno msg = do
  putStr $ msg ++ " (yes/no): "
  hFlush stdout
  ans <- getLine
  case ans of
    "yes" -> return True
    "no" -> return False
    _ -> (putStrLn $ ans ++ " is not a valid answer.") >> yesno msg


multiplayer :: (Statable a, Show a, Show c) => Bool -> b -> SolverRun a b c -> IO (a, Value)
multiplayer showmoves args solver = do
    rand <- newStdGen
    internal True $ solver rand
  where
    internal _ ((a, _), Terminal x) = return (a, x)
    internal first gs = do
      let ((a, f), Internal nss) = gs
          b = f args
      putStr $ "\n" ++ showGameState showmoves (not first) gs ++ "Make a move: "
      hFlush stdout
      move <- getLine
      let move' = if move == "ai" then show b else move
      if move' == "quit" then return (a, 0) else
        let valids = filter ((== move') . fst) $ nss in
        if null valids
          then putStrLn ("\nNot a valid move: " ++ move) >>
               internal first gs
          else internal (not first) $ snd $ head valids

playagain :: IO a -> IO a
playagain game = do
  x <- game
  again <- yesno "\nWould you like to play a new game?"
  if again then putStrLn "\n" >> playagain game else return x


singlemulti :: (Statable a, Show a, Show c) => Bool -> b -> SolverRun a b c -> IO (a, Value)
singlemulti showmoves args solver = do
  single <- yesno "Would you like to challenge the AI? "
  if single
    then singleplayerstart showmoves args solver
    else multiplayer showmoves args solver


singleplayerstart :: (Statable a, Show a, Show c) =>
                      Bool -> b -> SolverRun a b c -> IO (a, Value)
singleplayerstart showmoves args solver = do
    rand <- newStdGen
    let gs = solver rand
    first <- yesno "Would you like to start?"
    if first then singleplayeriter showmoves args gs else act gs where
      act ((a, _), Terminal x) = return (a, x)
      act ((a, f), Internal children) = do
        let b = f args
        putStrLn ""
        print a
        putStr $ "AI chose the move "
        print b
        let chosen = snd $ head $ filter ((== show b) . fst) $ children
        singleplayeriter showmoves args chosen


singleplayeriter :: (Statable a, Show a, Show c) =>
                     Bool -> b -> SolvedGameState a b c -> IO (a, Value)
singleplayeriter _ _ ((a, _), Terminal x) = return (a, x)
singleplayeriter showmoves args gs = do
  let ((a, b), Internal children) = gs
      first = isfirstplayer a
  putStr $ "\n" ++ showGameState showmoves (not first) gs ++ "Make a move: "
  hFlush stdout
  move <- getLine
  if move == "quit" then return (a, 0) else
    let valids = filter ((== move) . fst) $ children in
      if null valids
        then putStrLn ("\nNot a valid move: " ++ move) >>
             singleplayeriter showmoves args gs
        else act $ snd $ head valids where
          act ((a, _), Terminal x) = return (a, x)
          act ((a, f), Internal grandchildren) = do
            let b = f args
            putStrLn ""
            print a
            putStr "AI chose the move "
            print b
            singleplayeriter showmoves args $ snd $ head $ filter ((== show b) . fst) grandchildren

simulator :: (Show a, Show c) => b -> SolverRun a b c -> StdGen -> [String]
simulator args run rand = simulate $ run rand where
  simulate ((a, _), Terminal _) = [show a]
  simulate ((a, f), Internal children) = show a : simulate gs where
    move = show $ f args
    gs = snd $ head $ filter ((== move) . fst) $ children
