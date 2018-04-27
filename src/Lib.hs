{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Lib
    ( someFunc
    ) where

import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Data.Constraint
import           Control.Monad.Reader

someFunc :: IO ()
someFunc = putStrLn "someFunc"



-- LOGGING

data Logging (mc :: (* -> *) -> Constraint) =
  Logging { logMsgCapability :: forall m . mc m => String -> m () }

class HasLogging s a | s -> a where
    logging :: Lens' s a

-- | simplified logging service
class MonadLogging m where
  logMsg :: String -> m ()


instance (HasLogging env (Logging mc), Monad m, mc m)
    => MonadLogging (ReaderT env m) where

  logMsg msg = do
    fn <- asks (logMsgCapability . view logging)
    -- let x = env ^. logging
    --     fn = logMsgCapability x
    lift $ fn msg


-- METRICS

data Metrics (mc :: (* -> *) -> Constraint) =
  Metrics { incrCounterCapability :: forall m . mc m => CounterName -> m () }

class HasMetrics s a | s -> a where
    metrics :: Lens' s a

-- | simplified metrics service
class MonadMetrics m where
  incrementCounter :: CounterName -> m ()

instance (HasMetrics env (Metrics mc), Monad m, mc m)
    => MonadMetrics (ReaderT env m) where

  incrementCounter cn = do
    fn <- asks (incrCounterCapability . view metrics)
    lift $ fn cn



-- NEWTYPES

newtype CounterName = CounterName { unCounterName :: String }
