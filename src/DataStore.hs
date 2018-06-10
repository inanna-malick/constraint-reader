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
{-# LANGUAGE MonoLocalBinds #-}

module DataStore where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.Aeson as Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Constraint
import qualified Data.IORef as IORef
import           Data.List (isInfixOf)
import           Control.Monad.Reader
------------------------------------------------------------------------------
import Logging
import Metrics
import Types (Todo(..))
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


-- TODO:try imposing exceptT constraint via some of these instead of returning in Either?
writeTodo
  :: ( MonadDataStore DataStoreError m
     , MonadLogging m
     , MonadMetrics m
     , Monad m
     )
  => Todo
  -> m (Either DataStoreError ())
writeTodo todo = do
      logMsg $ LogMsg Debug $ "[DATASTORE] attempting to write todo: " ++ show todo
      incrementCounter $ CounterName "attempted-list-appends"

      -- business logic
      if ("heck" `isInfixOf` body todo)
        then do
          logMsg $ LogMsg Error "[DATASTORE] this is a christian server, no swearing!"
          incrementCounter $ CounterName "profanity-violations"
          -- TODO: better name or w/e
          pure $ Left $ ValidationError "no swearing allowed"

        -- if validation passes, append to the ioref's list
        else do
          incrementCounter $ CounterName "todo-list-appends"
          res <- appendToList Aeson.encode todo
          logMsg $ LogMsg Info $ "[DATASTORE] wrote todo: " ++ show todo
          pure res


readAllTodos
  :: ( MonadDataStore DataStoreError m
     , MonadLogging m
     , MonadMetrics m
     , Monad m
     )
  => m (Either DataStoreError [Todo])
readAllTodos = do
  -- TODO: kinda spurious logging maybe
  logMsg $ LogMsg Debug $ "[DATASTORE] attempting to read all todos"
  -- TODO: do logging, metrics, etc here, also move these fn's to Lib to replace biz logic
  -- TODO: have domain error type instead of using IMK stuff errywhere
  res <- readAllList (\x -> maybe (Left $ DecodeError x) Right $ Aeson.decode x)
  case res of
    Left (DecodeError x) -> do
      logMsg $ LogMsg Error $ "unable to decode value" ++ show x
      incrementCounter $ CounterName "decode-errors"
    Left (UnableToConnectError msg) ->
      logMsg $ LogMsg Error $ "unable to connect to data store: " ++ msg
    _ -> pure ()
  pure res

-- TODO: redis and shit
redisDataStore :: DataStore DataStoreError RedisDataStoreDeps
redisDataStore = DataStore
  { appendToListC = \_ _ -> undefined

  , readAllListC = \_ -> undefined
  }


-- | in-memory data store for use in tests, backed by 'IORef'
inMemoryDataStore :: IORef.IORef [ByteString] -> DataStore DataStoreError MonadIO
inMemoryDataStore ior = DataStore
  { appendToListC = \f elem' -> do
      liftIO $ IORef.modifyIORef ior (++ [f elem'])
      pure $ Right ()

  , readAllListC = \f -> do
      msgs <- liftIO $ IORef.readIORef ior
      pure $ traverse f msgs
  }

-- TODO: figure out how to throw these via exceptT? kinda tending towards nah
data DataStoreError
  = ValidationError String
  | DecodeError ByteString -- includes value that failed to decode
  | UnableToConnectError String
  deriving (Eq, Show)
