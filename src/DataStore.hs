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
      ) => MockDataStoreDeps m
instance ( MonadMetrics m
         , MonadLogging m
         , MonadIO m
         ) => MockDataStoreDeps m


-- TODO: try imposing exceptT constraint via some of these instead of returning in Either?
writeTodo
  :: ( MonadDataStore MockDataStoreError m
     , MonadLogging m
     , MonadMetrics m
     , Monad m
     )
  => Todo
  -> m (Either MockDataStoreError ())
writeTodo todo = do
      logInfo $ "[DATASTORE] attempting to write todo: " ++ show todo

      -- business logic
      if ("heck" `isInfixOf` body todo)
        then do
          logInfo "[DATASTORE] this is a christian server, no swearing!"
          incrementCounter $ CounterName "profanity-violations"
          -- TODO: better name or w/e
          pure $ Left $ MockDataStoreError "no swearing allowed"

        -- if validation passes, append to the ioref's list
        else do
          res <- appendToList Aeson.encode todo
          logInfo $ "[DATASTORE] wrote todo: " ++ show todo
          pure res


readAllTodos
  :: ( MonadDataStore MockDataStoreError m
     , MonadLogging m
     , Monad m
     )
  
  => m (Either MockDataStoreError [Todo])
readAllTodos = do
  -- TODO: kinda spurious logging maybe
  logInfo $ "[DATASTORE] attempting to read all todos"
  readAllList (maybe (Left $ MockDataStoreError "decode failed") Right . Aeson.decode)


-- TODO: move this logic -OUT- whole point is biz logic cares not for underlying impl, ioref should be treated as implementation detail, most logic should be in _functions that use datastore_, also can maybe use IOREF for test and, eg, redis for actual (w/ minimal gen'd json instances, etc)
-- mock data store, backed by ioref, includes toy validation logic to demonstrate error path
mockDataStore :: IORef.IORef [ByteString] -> DataStore MockDataStoreError MockDataStoreDeps
mockDataStore ior = DataStore
  { appendToListC = \f elem' -> do
      incrementCounter $ CounterName "list-appends"
      liftIO $ IORef.modifyIORef ior (++ [f elem'])
      pure $ Right ()

  , readAllListC = \f -> do
      logInfo $ "[DATASTORE] attempting to read all todos"
      msgs <- liftIO $ IORef.readIORef ior
      pure $ traverse f msgs
  }


newtype MockDataStoreError = MockDataStoreError { unMockDataStoreError :: String } deriving Show
