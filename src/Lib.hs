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

module Lib
    ( someFunc
    ) where

import           Control.Lens
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Trans.Class (lift)
import           Data.Constraint
import           Control.Monad.Reader

someFunc :: IO ()
someFunc = withEnv $ \env -> flip runReaderT env $ do
  logMsg "begin"
  incrementCounter $ CounterName "YOLO"
  logMsg "end"


-- TOP-LEVEL ENV BOILERPLATE
data Env =
  Env { envLogging :: Logging MonadIO, envMetrics :: Metrics MonadLogging}

instance HasMetrics Env (Metrics MonadLogging) where
    metrics = lens envMetrics (\s a -> s { envMetrics = a })

instance HasLogging Env (Logging MonadIO) where
    logging = lens envLogging (\s a -> s { envLogging = a })

withEnv :: (Env -> IO a) -> IO a
withEnv k = k env
  where
    env = Env {envLogging = stdoutLogging, envMetrics = mockMetrics}

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
stdoutLogging = Logging {logMsgCapability = liftIO . putStrLn}

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
  { incrCounterCapability = \cn -> logMsg $ "increment :" ++ show cn
  }

-- NEWTYPES

newtype CounterName = CounterName { unCounterName :: String } deriving Show

data Foo = Foo
data Baz = Baz
