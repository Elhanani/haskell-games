{-# LANGUAGE MultiParamTypeClasses
           , AllowAmbiguousTypes
           , FlexibleInstances
           , BangPatterns
             #-}

module Cache (Cache(..), Hashable,
              runNoCache, runMapCache, runHashMapCache) where

import Data.Hashable
import Control.Monad.ST
import Control.Monad.Reader
import Data.Functor.Identity
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

  readCacheDefault k dv = do
    mv <- readCache k
    return $ case mv of
      Just jv -> jv
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
  writeCache !k !v = get >>= put . (M.insert k v) >> return ()
  filterCache !f = get >>= put . (M.filterWithKey f) >> return ()

runMapCache :: (k -> State (M.Map k v) v) -> k -> v
runMapCache f k = fst $ runState (f k) M.empty


-- | State Monad with HashMap

instance (Hashable k, Eq k) => Cache (State (H.HashMap k v)) k v where
  readCache !k = get >>= return . (H.lookup k)
  readCacheDefault !k !v = get >>= return . (H.lookupDefault v k)
  writeCache !k !v = get >>= put . (H.insert k v) >> return ()
  filterCache !f = get >>= put . (H.filterWithKey f) >> return ()

runHashMapCache :: (k -> State (H.HashMap k v) v) -> k -> v
runHashMapCache f k = fst $ runState (f k) H.empty

-- | ST Monad with a partial hashtable

data PartialRef s k v = PartialRef {array :: STA.STArray s Int (Maybe v)
                                  , overwrite :: ((k, v), (k, v)) -> Bool
                                  , size :: Int
                                    }

instance (Hashable k) => Cache (ReaderT (PartialRef s k v) (ST s)) k v where
  readCache !k = undefined
  readCacheDefault !k !v = undefined
  writeCache !k !v = undefined
  filterCache !f = undefined
