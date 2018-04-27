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
import DataStore
import Logging
import Metrics
import Types (Todo(..))
------------------------------------------------------------------------------

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

