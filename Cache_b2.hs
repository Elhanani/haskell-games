{-# LANGUAGE MultiParamTypeClasses
           , AllowAmbiguousTypes
           , FlexibleInstances
           , FlexibleContexts
           , BangPatterns
           , NamedFieldPuns
           , ExistentialQuantification
             #-}

module Cache (Cache(..), Hashable,
              runNoCache, runMapCache, runHashMapCache) where

import Data.Hashable
import Control.Monad.ST
import Control.Monad.Reader
import Data.Functor.Identity
import Data.Maybe
import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import qualified Data.Array.ST as STA
import Control.Monad.State
import Control.Monad.ST


-- | Different types of transposition tables

class (Monad m) => Cache m k v where
  readCache :: k -> m (Maybe v)
  readCacheDefault :: k -> v -> m v
  writeCache :: k -> v -> m ()
  filterCache :: (k -> v -> Bool) -> m ()

  readCacheDefault !k dv = do
    !mv <- readCache k
    return $! case mv of
      Just !jv -> jv
      Nothing -> dv


-- | Trivial implementation (i.e no table)

instance Cache Identity k v where
  -- runEmptyCache = runIdentity
  readCache _ = return Nothing
  readCacheDefault _ a = return a
  writeCache _ _ = return ()
  filterCache _ = return ()

runNoCache :: (k -> Identity v) -> k -> v
runNoCache f k = runIdentity (f k)


-- | State monad with a Map

instance (Ord k) => Cache (State (M.Map k v)) k v where
  readCache !k = get >>= return . (M.lookup k)
  readCacheDefault !k !v = get >>= return . (M.findWithDefault v k)
  writeCache !k !v = get >>= put . (M.insert k v)
  filterCache !f = get >>= put . (M.filterWithKey f)

runMapCache :: (k -> State (M.Map k v) v) -> k -> v
runMapCache f k = fst $ runState (f k) M.empty


-- | State Monad with HashMap

instance (Hashable k, Eq k) => Cache (State (H.HashMap k v)) k v where
  readCache !k = get >>= return . (H.lookup k)
  readCacheDefault !k !v = get >>= return . (H.lookupDefault v k)
  writeCache !k !v = get >>= put . (H.insert k v)
  filterCache !f = get >>= put . (H.filterWithKey f)

runHashMapCache :: (k -> State (H.HashMap k v) v) -> k -> v
runHashMapCache f k = fst $ runState (f k) H.empty


-- | ST Monad with a partial hashtable

data SingleHashRef s k v = SingleHashRef {singleArray :: STA.STArray s Int (Maybe (k, v))
                                        , singleOverwrite :: (k, v) -> (k, v) -> Bool
                                        , singleSize :: !Int
                                          }

data SingleHashTable s k v a = --forall s.
  SingleHashTable {runSingleHashTable :: SingleHashRef s k v -> ST s a}

instance Functor (SingleHashTable s k v) where
  fmap f (SingleHashTable {runSingleHashTable = g}) =
    SingleHashTable {runSingleHashTable = fmap f . g}

instance Applicative (SingleHashTable s k v) where
  pure a = SingleHashTable {runSingleHashTable = const $ pure a}
  (SingleHashTable {runSingleHashTable = f}) <*> (SingleHashTable {runSingleHashTable = g}) =
    SingleHashTable {runSingleHashTable = fg} where
      fg shr = (f shr) <*> (g shr)

instance Monad (SingleHashTable s k v) where
  return a = SingleHashTable {runSingleHashTable = const $ return a}
  (SingleHashTable {runSingleHashTable = f}) >>= g =
    SingleHashTable {runSingleHashTable = gf} where
      gf shr = (f shr) >>= (\a -> runSingleHashTable (g a) shr)

--
-- instance (Hashable k, Eq k) => Cache (ReaderT (SingleHashRef s k v) (ST s)) k v where
--   readCache !k = do
--     !SingleHashRef {array, size} <- ask
--     let f (Just (k, v)) = Just v
--         f _ = Nothing
--     fmap f $ lift $ STA.readArray array (mod (hash k) size)
--   writeCache !k !v = do
--     !SingleHashRef {array, overwrite, size} <- ask
--     !current <- lift $ STA.readArray array (mod (hash k) size)
--     let f Nothing = True
--         f (Just (k', v')) = k' == k || overwrite (k', v') (k, v)
--     if f current
--       then lift $ STA.writeArray array (mod (hash k) size) (Just (k, v))
--       else return ()
--   filterCache !f = do
--     !SingleHashRef {array, size} <- ask
--     let g n = do
--               current <- STA.readArray array (mod n size)
--               if isNothing current then return () else let Just (k, v) = current in
--                 if f k v then return () else STA.writeArray array (mod n size) Nothing
--     lift $ mapM_ g [0..size-1]
--


runSingleHashCache :: Int -> ((k, v) -> (k, v) -> Bool) ->
                      (k -> (SingleHashTable s k v) v) -> k -> v
runSingleHashCache singleSize singleOverwrite f k = runST $ do
  singleArray <- STA.newArray (0, singleSize-1) Nothing
  runSingleHashTable (f k) (SingleHashRef {singleArray, singleSize, singleOverwrite})
