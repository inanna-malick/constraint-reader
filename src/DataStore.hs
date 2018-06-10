{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}

module DataStore where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Reader
import           Data.ByteString.Lazy (ByteString)
import           Data.Constraint
import qualified Data.IORef as IORef
import qualified Database.Redis.IO as Redis
------------------------------------------------------------------------------
import Logging
import Metrics
------------------------------------------------------------------------------

-- DATASTORE BOILERPLATE

data DataStore err (mc :: (* -> *) -> Constraint) = DataStore
  { appendToListC :: forall m x . mc m => (x -> ByteString) -> x -> m (Either err ())
  , readAllListC :: forall m x . mc m => (ByteString -> Either err x) -> m (Either err [x])
  }

class HasDataStore s a | s -> a where
    datastore :: Lens' s a

-- | simplified datastore service
-- TODO: see if I can drop and just use caps constraints
class MonadDataStore err m where
  appendToList :: (x -> ByteString) -> x -> m (Either err ())
  readAllList :: (ByteString -> Either err x) -> m (Either err [x])


instance (HasDataStore env (DataStore err mc), MonadReader env m, mc m) => MonadDataStore err m where
  appendToList f elem'= do
    fn <- asks (appendToListC . view datastore)
    fn f elem'

  readAllList f = do
    fn <- asks (readAllListC . view datastore)
    fn f

-- DATASTORE SERVICE MOCK IMPL

class ( MonadMetrics m
      , MonadLogging m
      , MonadIO m
      ) => RedisDataStoreDeps m
instance ( MonadMetrics m
         , MonadLogging m
         , MonadIO m
         ) => RedisDataStoreDeps m


-- TODO: redis and shit
redisDataStore :: Redis.Pool -> DataStore DataStoreError RedisDataStoreDeps
redisDataStore pool = DataStore
-- TODO: catch and return wrapped redis errors
  { appendToListC = \encode elem' -> do
      todoListLen <- Redis.runRedis pool
        . Redis.commands
        . Redis.rpush k
        . pure $ encode elem'
      -- TODO: incr metrics and such, have access to that + logger
      logMsg $ LogMsg Debug $
        "appended elem to list:" ++ show k ++
        ", list now contains " ++ show todoListLen ++ " values"
      pure $ Right ()

  , readAllListC = \decode -> do
      -- TODO: incr metrics and such
      raw <- Redis.runRedis pool . Redis.commands $ Redis.lrange k 0 (-1)
      pure $ traverse decode raw
  }
  where
    k :: Redis.Key
    k = "my-todos"


-- | in-memory data store for use in tests, backed by 'IORef'
inMemoryDataStore :: IORef.IORef [ByteString] -> DataStore DataStoreError MonadIO
inMemoryDataStore ior = DataStore
  { appendToListC = \encode elem' -> do
      liftIO $ IORef.modifyIORef ior (++ [encode elem'])
      pure $ Right ()

  , readAllListC = \encode -> do
      msgs <- liftIO $ IORef.readIORef ior
      pure $ traverse encode msgs
  }

-- TODO: figure out how to throw these via exceptT? kinda tending towards nah
data DataStoreError
  = ValidationError String
  | DecodeError ByteString -- includes value that failed to decode
  | UnableToConnectError String
  deriving (Eq, Show)
