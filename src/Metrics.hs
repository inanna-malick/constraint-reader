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
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Trans.Class (lift)
import           Data.Constraint (Constraint)
import           Data.Foldable (traverse_)
import qualified Data.IORef as IORef
import           Data.List (isInfixOf)
import           Control.Monad.Reader
------------------------------------------------------------------------------
import Logging
------------------------------------------------------------------------------

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


-- TYPES
newtype CounterName = CounterName { unCounterName :: String } deriving Show
