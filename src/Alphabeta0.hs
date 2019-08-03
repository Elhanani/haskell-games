{-# LANGUAGE ExistentialQuantification, NamedFieldPuns, RankNTypes, BangPatterns #-}

module Alphabeta0 where --()

import SolverDefs
import Cache
import Data.Maybe
import Data.List
import System.Random

-- | alpha, beta are the broadest alpha/beta for the search
--   oactions is the ordered actions list for search
--   lessevil is a function to select an option when it's impossible to guarantee a win
data ABParams gs = ABParams {alpha :: {-# UNPACK #-} !Value, beta :: {-# UNPACK #-} !Value,
                             oactions :: gs -> [(String, gs)],
                             lessevil :: gs -> [String] -> IO String}

defaultABParams :: forall gs. (GameState gs) => ABParams gs
defaultABParams = ABParams {alpha = -1, beta = 1, oactions = actions,
                            lessevil = const randomAction}

isSufficient :: GameState gs => Player -> ABParams gs -> ABValue -> Bool
isSufficient Maximizer (ABParams {beta}) (Just val, _) = val >= beta
isSufficient Minimizer (ABParams {alpha}) (_, Just val) = val <= alpha
isSufficient _ _ _ = False

-- | Lowerbound and upperbound
type ABValue = (Maybe Value, Maybe Value)

type ABAction gs = (String, ABNode gs)

data ABNode gs = ABNode {gameState :: gs,
                         value :: ABValue,
                         sufficient :: Maybe (ABAction gs),
                         unknowns :: [ABAction gs],
                         knowns :: [ABAction gs],
                         worst :: [ABAction gs],
                         best :: [ABAction gs]}

data ABSolvedGame = forall gs. (GameState gs) =>
  ABSolvedGame {abNode :: ABNode gs,
                abParams :: ABParams gs}

instance Show ABSolvedGame where
  show ABSolvedGame {abNode = ABNode {gameState}} = show gameState

instance GameState ABSolvedGame where
  player ABSolvedGame {abNode = ABNode {gameState}} = player gameState
  terminal ABSolvedGame {abNode = ABNode {gameState}} = terminal gameState
  maxdepth ABSolvedGame {abNode = ABNode {gameState}} = maxdepth gameState
  actions ABSolvedGame {abParams, abNode = ABNode {best, worst, knowns, unknowns, sufficient}} =
    map mkSolvedGame (unknowns ++ knowns ++ best ++ worst ++ maybeToList sufficient) where
      mkSolvedGame (str, abNode) = (str, ABSolvedGame {abParams, abNode})

instance SolvedGameState ABSolvedGame where
  action ABSolvedGame {abParams, abNode} = getAction $ findBest abParams abNode where
    getAction (ABNode {sufficient = Just (str, node)}) =
      return (str, ABSolvedGame {abParams, abNode = node})
    getAction (ABNode {gameState, best=best@(_:_)}) = do
      str <- (lessevil abParams) gameState $ map fst best
      return (str, ABSolvedGame {abParams, abNode = fromJust $ lookup str best})
    getAction (ABNode {gameState, worst}) = do
      str <- (lessevil abParams) gameState $ map fst worst
      return (str, ABSolvedGame {abParams, abNode = fromJust $ lookup str worst})

-- This should look into "actions" first to determine if there is a "sufficient" state
-- among the already cached values.
mkNode :: GameState gs => gs -> ABParams gs -> ABNode gs
mkNode gameState params@(ABParams {oactions}) = ABNode {gameState, value = (Nothing, Nothing),
                                                        sufficient = Nothing, unknowns, knowns = [],
                                                        best = [], worst = []} where
  unknowns = map (\(str, x) -> (str, mkNode x params)) $ oactions gameState

findBest :: GameState gs => ABParams gs -> ABNode gs -> ABNode gs
findBest _ node@ABNode{unknowns=[], best=(_:_)} = node
-- findBest params node = if isSufficient params node
--   then node
--   else findBest $ advanceNode params node where
--     isSufficient params (ABNode {gameState, value}) =


-- if sufficient exists and is as good as the requirement then do nothing
-- if suffiecent doesn't exist (or not as good as required) deplete the unknowns
-- if nothing is unknown then do nothing
