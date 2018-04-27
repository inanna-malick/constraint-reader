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

-- lol wtf is this, warning msg suggested it
-- ok it makes the warnings go away, cool
{-# LANGUAGE MonoLocalBinds #-}

module DataStore where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Trans.Class (lift)
import           Data.Constraint
import           Data.Foldable (traverse_)
import qualified Data.IORef as IORef
import           Data.List (isInfixOf)
import           Control.Monad.Reader
------------------------------------------------------------------------------
import Logging
import Metrics
import Types (Todo(..))
------------------------------------------------------------------------------


-- DATASTORE BOILERPLATE
-- supports : writing todo note, reading all todo notes
-- note to self: have parameterized over.. idk, error type? then just Constrain that to exception and dot dot dot

data DataStore err (mc :: (* -> *) -> Constraint) = DataStore
  { writeTodoCapability :: forall m . mc m => Todo -> m (Either err ())
  , readAllTodosCapability :: forall m . mc m => m (Either err [Todo])
  }

class HasDataStore s a | s -> a where
    datastore :: Lens' s a

-- | simplified datastore service
class MonadDataStore err m where
  writeTodo :: Todo -> m (Either err ())
  readAllTodos :: m (Either err [Todo])


instance (HasDataStore env (DataStore err mc), MonadReader env m, mc m) => MonadDataStore err m where
  writeTodo todo = do
    fn <- asks (writeTodoCapability . view datastore)
    fn todo

  readAllTodos = do
    fn <- asks (readAllTodosCapability . view datastore)
    fn

-- DATASTORE SERVICE MOCK IMPL

class ( MonadMetrics m
      , MonadLogging m
      , MonadIO m
      ) => MockDataStoreDeps m
instance ( MonadMetrics m
         , MonadLogging m
         , MonadIO m
         ) => MockDataStoreDeps m


-- mock data store, backed by ioref
mockDataStore :: IORef.IORef [Todo] -> DataStore MockDataStoreError MockDataStoreDeps
mockDataStore ior = DataStore
  { writeTodoCapability = \todo -> do
      logMsg $ "[DATASTORE] attempting to write todo: " ++ show todo

      -- arbitrary and artificial validation check to demonstrate error resp path
      if ("heck" `isInfixOf` body todo)
        then do
          logMsg "[DATASTORE] this is a christian server, no swearing!"
          incrementCounter $ CounterName "datastore-errors"
          pure $ Left $ MockDataStoreError "no swearing allowed"

        -- if validation passes, append to the ioref's list
        else do
          liftIO $ IORef.modifyIORef ior (++ [todo])
          logMsg $ "[DATASTORE] wrote todo: " ++ show todo
          incrementCounter $ CounterName "datastore-writes"
          pure $ Right ()

  , readAllTodosCapability = do
      logMsg $ "[DATASTORE] attempting to read all todos"
      msgs <- liftIO $ IORef.readIORef ior

      -- another totally artificial and arbitrary demonstration of error path
      if (length msgs > 3)
        then do
          logMsg "[DATASTORE] idk buffer flow or something"
          incrementCounter $ CounterName "datastore-errors"
          pure $ Left $ MockDataStoreError "buffer overflow (?)"
        else
          pure $ Right msgs
  }


newtype MockDataStoreError = MockDataStoreError { unMockDataStoreError :: String } deriving Show
