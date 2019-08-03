module Xorshift64 (newXorshift64) where

import Data.Int
import Data.Bits
import System.Random

xorshift64 :: (Int, Int, Int) -> Int64 -> Int64
xorshift64 (s1, s2, s3) state0 = state3 where
  state1 = xor state0 $ shift state0 s1
  state2 = xor state1 $ shift state1 s2
  state3 = xor state2 $ shift state2 s3

newtype Xorshift64 = Xorshift64 Int64 deriving Show

instance RandomGen Xorshift64 where
  next (Xorshift64 state) = (fromEnum state', Xorshift64 state') where
    state' = xorshift64 (13, -7, 17) state

  genRange (Xorshift64 state) = (minBound, maxBound)

  split (Xorshift64 state) = (Xorshift64 state1, Xorshift64 state2) where
    state1 = xorshift64 (12, -25, 27) $ rotate state 19
    state2 = xorshift64 (-12, 25, -27) $ rotate state 5

newXorshift64 :: IO Xorshift64
newXorshift64 = (Xorshift64 . ((.|.) 1) . toEnum) <$> randomIO
