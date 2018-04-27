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



data Logging (mc :: (* -> *) -> Constraint) = Logging { logMsgCapability :: forall m . mc m => String -> m () }

class HasLogging s a | s -> a where
    logging :: Lens' s a

-- | simplified logging service
class MonadLogging m where
  logMsg :: String -> m ()


instance (HasLogging env (Logging mc), MonadIO m, mc m) => MonadLogging (ReaderT env m) where
-- instance (HasLogging env (Logging mc), MonadIO m, mc m) => MonadLogging (ReaderT env m) where
  logMsg msg = do
    (env :: env) <- ask
    let x = env ^. logging
        fn = logMsgCapability x
    lift $ fn msg



-- newtype CounterName = CounterName { unCounterName :: String }

-- class HasMetrics env where
--     getMetrics :: env -> (CounterName -> IO ())

-- -- | simplified metrics service
-- class MonadMetrics m where
--   incrementCounter :: CounterName -> m ()

-- instance (HasMetrics env, MonadIO m) => MonadMetrics (ReaderT env m) where
--   incrementCounter cn = do
--     fn <- asks getMetrics
--     liftIO $ fn cn
