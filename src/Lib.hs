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

module Lib
    ( someFunc
    ) where

import           Control.Lens
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Trans.Class (lift)
import           Data.Constraint
import           Data.Foldable (traverse_)
import qualified Data.IORef as IORef
import           Data.List (isInfixOf)
import           Control.Monad.Reader

someFunc :: IO ()
someFunc = withEnv $ \env -> flip runReaderT env $ do
  let todos1 = [Todo "task 1" "do that thing", Todo "malformed" ""]
  res1 <- doBizLogic todos1
  logMsg $ "res1: " ++ show res1
  let todos2 = [Todo "task 2" "do that other thing", Todo "task 3" "oh heck"]
  res2 <- doBizLogic todos2
  logMsg $ "res2: " ++ show res2
  let todos3 = [Todo "task 3" "do that third thing", Todo "task 4" "fix buffer overflow when size of tasks > 3"]
  res3 <- doBizLogic todos3
  logMsg $ "res3: " ++ show res3



-- accept a batch of todos, do some validation and drop any invalid while logging,
-- write all valid to data store, read out new state of data store
doBizLogic
  :: ( MonadDataStore err m
     , MonadLogging m
     , MonadMetrics m
     , Monad m
     )
  => [Todo]
  -> m (Either err [Todo])
doBizLogic ts = do
    errs <- flip traverse ts $ \t ->
              if (validateTodo t)
                then writeTodo t
                else do
                  -- just drop any msgs with blank fields (todo: drop?)
                  logMsg $ "[BIZLOGIC ERROR] invalid todo msg: " ++ show t
                  pure $ Right ()
    let errs' = traverse id errs
    -- todo: runExceptT or w/e
    case errs' of
      Right _ -> readAllTodos
      Left err -> pure $ Left err

  where
    validateTodo t = (not . null . description $ t) && (not . null . body $ t)

-- TOP-LEVEL ENV BOILERPLATE
data Env =
  Env { envLogging :: Logging MonadIO
      , envMetrics :: Metrics MonadLogging
      , envDataStore :: DataStore MockDataStoreError MockDataStoreDeps
      }

instance HasMetrics Env (Metrics MonadLogging) where
    metrics = lens envMetrics (\s a -> s { envMetrics = a })

instance HasLogging Env (Logging MonadIO) where
    logging = lens envLogging (\s a -> s { envLogging = a })


instance HasDataStore Env (DataStore MockDataStoreError MockDataStoreDeps) where
    datastore = lens envDataStore (\s a -> s { envDataStore = a })

withEnv :: (Env -> IO a) -> IO a
withEnv k = do
  ior <- IORef.newIORef []
  k $ env ior
  where
    env ior = Env { envLogging = stdoutLogging
                  , envMetrics = mockMetrics
                  , envDataStore = mockDataStore ior
                  }

-- LOGGING BOILERPLATE

data Logging (mc :: (* -> *) -> Constraint) =
  Logging { logMsgCapability :: forall m . mc m => String -> m () }

class HasLogging s a | s -> a where
    logging :: Lens' s a

-- | simplified logging service
class MonadLogging m where
  logMsg :: String -> m ()


instance (HasLogging env (Logging mc), MonadReader env m, mc m) => MonadLogging m where
  logMsg msg = do
    fn <- asks (logMsgCapability . view logging)
    fn msg

-- LOGGING SERVICE IMPL

stdoutLogging :: Logging MonadIO -- only requirement is the ability to do IO, could also have MonadResource for file handle (todo: that)
stdoutLogging = Logging {logMsgCapability = liftIO . putStrLn . ("logmsg: " ++)}

-- METRICS BOILERPLATE

data Metrics (mc :: (* -> *) -> Constraint) =
  Metrics { incrCounterCapability :: forall m . mc m => CounterName -> m () }

class HasMetrics s a | s -> a where
    metrics :: Lens' s a

-- | simplified metrics service
class MonadMetrics m where
  incrementCounter :: CounterName -> m ()


instance (HasMetrics env (Metrics mc), MonadReader env m, mc m) => MonadMetrics m where
  incrementCounter cn = do
    fn <- asks (incrCounterCapability . view metrics)
    fn cn

-- METRICS SERVICE IMPL

mockMetrics :: Metrics MonadLogging -- only requirement is ability to log 'counter increment' ops
mockMetrics = Metrics
  { incrCounterCapability = \cn -> logMsg $ "(MOCK METRICS) increment:" ++ show cn
  }


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


-- NEWTYPES

newtype CounterName = CounterName { unCounterName :: String } deriving Show

data Todo = Todo { description :: String, body :: String } deriving (Show, Eq, Ord)

newtype MockDataStoreError = MockDataStoreError { unMockDataStoreError :: String } deriving Show
