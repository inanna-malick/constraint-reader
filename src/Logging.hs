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

module Logging where

import           Control.Lens
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Writer (MonadWriter)
import qualified Control.Monad.Writer as W
import           Data.Constraint (Constraint)
import           Control.Monad.Reader

-- LOGGING BOILERPLATE

data Logging (mc :: (* -> *) -> Constraint) =
  Logging { logInfoC :: forall m . mc m => String -> m () }

class HasLogging s a | s -> a where
    logging :: Lens' s a

-- | simplified logging service
class MonadLogging m where
  logInfo :: String -> m ()


instance (HasLogging env (Logging mc), MonadReader env m, mc m) => MonadLogging m where
  logInfo msg = do
    fn <- asks (logInfoC . view logging)
    fn msg

-- LOGGING SERVICE IMPL

stdoutLogging :: Logging MonadIO -- only requirement is the ability to do IO, could also have MonadResource for file handle (todo: that)
stdoutLogging = Logging {logInfoC = liftIO . putStrLn . ("logmsg: " ++)}


-- test logging service that uses underlying 'Writer' monad to write log messages
testLogging :: Logging (MonadWriter String)
testLogging = Logging {logInfoC = W.tell}
