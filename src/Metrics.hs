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

module Metrics where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.State (MonadState)
import qualified Control.Monad.State as S
import           Data.Constraint (Constraint)
import qualified Data.Map as M
import           Control.Monad.Reader
------------------------------------------------------------------------------
import Logging
------------------------------------------------------------------------------

-- METRICS BOILERPLATE

data Metrics (mc :: (* -> *) -> Constraint) =
  Metrics { incrementCounterC :: forall m . mc m => CounterName -> m () }

class HasMetrics s a | s -> a where
    metrics :: Lens' s a

-- | simplified metrics service
class MonadMetrics m where
  incrementCounter :: CounterName -> m ()


instance (HasMetrics env (Metrics mc), MonadReader env m, mc m) => MonadMetrics m where
  incrementCounter cn = do
    fn <- asks (incrementCounterC . view metrics)
    fn cn

-- METRICS SERVICE IMPL
-- TODO: also back w/ redis, easy enough to do
mockMetrics :: Metrics MonadLogging -- only requirement is ability to log 'counter increment' ops
mockMetrics = Metrics
  { incrementCounterC = \cn -> logInfo $ "(MOCK METRICS) increment:" ++ show cn
  }

-- test metrics service that uses underlying 'State' monad to store map of metrics
testMetrics :: Metrics (MonadState (M.Map CounterName Int))
testMetrics = Metrics
  { incrementCounterC = \cn -> S.modify (M.insertWith (+) cn 1)
  }

-- TYPES
newtype CounterName = CounterName { unCounterName :: String } deriving (Eq, Ord, Show)
