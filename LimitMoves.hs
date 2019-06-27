{-# LANGUAGE ExistentialQuantification
           , NamedFieldPuns
           , RankNTypes
           , BangPatterns
             #-}

module LimitMoves (removeSymmetries) where

import SolverDefs
import System.Random
import qualified Data.Set as S

data LimitInitialMoves =
    forall gs. (GameState gs) => LimitInitialMoves (gs, S.Set String)
  | forall gs. (GameState gs) => NoLimitInitialMoves gs

instance Show LimitInitialMoves where
  show !(LimitInitialMoves (!gs, _)) = show gs
  show !(NoLimitInitialMoves !gs) = show gs

instance GameState LimitInitialMoves where
  firstplayer !(LimitInitialMoves (!gs, _)) = firstplayer gs
  firstplayer !(NoLimitInitialMoves !gs) = firstplayer gs
  terminal !(LimitInitialMoves (!gs, _)) = terminal gs
  terminal !(NoLimitInitialMoves !gs) = terminal gs
  actions !(NoLimitInitialMoves !gs) = map mf $ actions gs where
    mf (!str, !ngs) = (str, mkLimitInitialMoves ngs)
  actions !(LimitInitialMoves (!gs, !allowed)) =
    map mf $ filter ff $ actions gs where
      mf !(!str, !ngs) = (str, mkLimitInitialMoves ngs)
      ff !(str, _) = S.member str allowed
  numactions !(NoLimitInitialMoves !gs) = numactions gs
  numactions !gs = length $! actions gs
  act !(NoLimitInitialMoves !gs) str = fmap mkLimitInitialMoves $! act gs str
  act !gs !str = lookup str $! actions gs where

mkLimitInitialMoves :: GameState gs => gs -> LimitInitialMoves
mkLimitInitialMoves !gs = NoLimitInitialMoves gs

removeSymmetries :: (GameState gs, RandomGen rand) => [[String]] -> rand -> gs -> LimitInitialMoves
removeSymmetries symmetries rand gs = LimitInitialMoves (gs, allowed) where
  !rs = randoms rand
  f !n ![x] = x
  f !n !xs = xs !! (mod n (length xs -1))
  !allowed = S.fromList $! zipWith f rs symmetries
