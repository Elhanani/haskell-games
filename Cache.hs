{-# LANGUAGE MultiParamTypeClasses
           , AllowAmbiguousTypes
           , FlexibleInstances
           , FlexibleContexts
           , BangPatterns
           , NamedFieldPuns
           , ExistentialQuantification
           , RankNTypes
             #-}

module Cache (Cache(..), Hashable,
              NoCacheRef, runNoCache,
              MapCacheRef, runMapCache,
              HashMapCacheRef, runHashMapCache,
              SingleHashRef, singleHashCacheST) where

import Data.Hashable
import Data.Proxy
import Control.Monad.ST
import Data.Functor.Identity
import Data.Maybe
import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import qualified Data.Array.MArray as MA
import qualified Data.Array.ST as STA
import Control.Monad.State
import Control.Monad.ST

-- runNoCache = undefined
-- runMapCache = undefined
-- runHashMapCache = undefined


-- | Pure types of transposition tables

-- data Cache' k v = (forall m. Monad m) =>
--   Cache' {readCache :: r -> k -> m (Maybe v)
--        , readCacheDefault :: r -> k -> v -> m v
--        , writeCache :: r -> k -> v -> m ()
--        , filterCache :: r -> (k -> v -> Bool) -> m ()
--        , runCache :: m a -> (a, kv)
--        , newRef :: m r
--        , loadRef :: kv -> m r
-- }

class Monad m => Cache m r k v where
  readCache :: r -> k -> m (Maybe v)
  readCacheDefault :: r -> k -> v -> m v
  writeCache :: r -> k -> v -> m ()
  filterCache :: r -> (k -> v -> Bool) -> m ()

  readCacheDefault !r !k dv = do
    !mv <- readCache r k
    return $! case mv of
      Just !jv -> jv
      Nothing -> dv

-- class (Cache (ReaderT r (ST s)) k v) => STHashRef r s k v

-- | Trivial implementation (i.e no table)

data NoCacheRef k v = NoCacheRef

instance Cache Identity (NoCacheRef k v) k v where
  readCache _ _ = return Nothing
  readCacheDefault _ _ a = return a
  writeCache _ _ _ = return ()
  filterCache _ _ = return ()

runNoCache :: (Identity (NoCacheRef k v) -> k -> Identity v) -> k -> v
runNoCache f k = runIdentity $ f (return NoCacheRef) k


-- | State monad with a Map

data MapCacheRef k v = MapCacheRef

instance (Ord k) => Cache (State (M.Map k v)) (MapCacheRef k v) k v where
  readCache _ !k = get >>= return . (M.lookup k)
  readCacheDefault _ !k !v = get >>= return . (M.findWithDefault v k)
  writeCache _ !k !v = get >>= put . (M.insert k v)
  filterCache _ !f = get >>= put . (M.filterWithKey f)

runMapCache :: (State (M.Map k v) (MapCacheRef k v) -> k -> State (M.Map k v) v) -> k -> v
runMapCache f k = fst $ runState (f (return MapCacheRef) k) M.empty


-- | State Monad with HashMap

data HashMapCacheRef k v = HashMapCacheRef

instance (Hashable k, Eq k) => Cache (State (H.HashMap k v)) (HashMapCacheRef k v) k v where
  readCache _ !k = get >>= return . (H.lookup k)
  readCacheDefault _ !k !v = get >>= return . (H.lookupDefault v k)
  writeCache _ !k !v = get >>= put . (H.insert k v)
  filterCache _ !f = get >>= put . (H.filterWithKey f)

runHashMapCache :: (State (H.HashMap k v) (HashMapCacheRef k v) -> k ->
                    State (H.HashMap k v) v) -> k -> v
runHashMapCache f k = fst $ runState (f (return HashMapCacheRef) k) H.empty


-- | ST Monad with a partial hashtable

data SingleHashRef a k v =
  SingleHashRef {singleArray :: a Int (Maybe (k, v))
               , singleOverwrite :: (k, v) -> (k, v) -> Bool
               , singleSize :: !Int
                }

instance (Hashable k, Eq k, Monad m, MA.MArray a (Maybe (k, v)) m) =>
          Cache m (SingleHashRef a k v) k v where
  readCache !ref !k = do
    let !SingleHashRef {singleArray, singleSize} = ref
    let f (Just (k', v)) = if k == k' then Just v else Nothing
        f _ = Nothing
    fmap f $ MA.readArray singleArray (mod (hash k) singleSize)
  writeCache !ref !k !v = do
    let !SingleHashRef {singleArray, singleOverwrite, singleSize} = ref
    !current <- MA.readArray singleArray (mod (hash k) singleSize)
    let f Nothing = True
        f (Just (k', v')) = k' == k || singleOverwrite (k', v') (k, v)
    if f current
      then MA.writeArray singleArray (mod (hash k) singleSize) (Just (k, v))
      else return ()
  filterCache !ref !f = do
    let !SingleHashRef {singleArray, singleSize} = ref
    let g n = do
              current <- MA.readArray singleArray (mod n singleSize)
              if isNothing current then return () else let Just (k, v) = current in
                if f k v then return () else MA.writeArray singleArray (mod n singleSize) Nothing
    mapM_ g [0..singleSize-1]


singleHashCacheST :: Int -> ((k, v) -> (k, v) -> Bool) ->
                     ((ST s) (SingleHashRef (STA.STArray s) k v) -> k -> (ST s) v) -> k -> ST s v
singleHashCacheST singleSize singleOverwrite f k = f mref k where
  !mref = do
    singleArray <- STA.newArray (0, singleSize-1) Nothing
    return SingleHashRef {singleArray, singleOverwrite, singleSize}
