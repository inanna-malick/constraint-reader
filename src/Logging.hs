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

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Writer (MonadWriter)
import qualified Control.Monad.Writer as W
import           Data.Constraint (Constraint)
import           Control.Monad.Reader
import qualified System.Logger as TinyLog
------------------------------------------------------------------------------


data LogLevel = Debug | Info | Error
  deriving (Eq, Ord, Show)

data LogMsg = LogMsg
 { logLevel :: LogLevel
 , logBody  :: String
 } deriving (Eq, Ord, Show)

-- LOGGING BOILERPLATE

data Logging (mc :: (* -> *) -> Constraint) =
  Logging { logMsgC :: forall m . mc m => LogMsg -> m () }

class HasLogging s a | s -> a where
    logging :: Lens' s a

-- | simplified logging service
class MonadLogging m where
  logMsg :: LogMsg -> m ()


instance (HasLogging env (Logging mc), MonadReader env m, mc m) => MonadLogging m where
  logMsg msg = do
    fn <- asks (logMsgC . view logging)
    fn msg

-- LOGGING SERVICE IMPL

tinylogLogging :: TinyLog.Logger -> Logging MonadIO -- only requirement is the ability to do IO, could also have MonadResource for file handle (todo: that)
tinylogLogging tl = Logging {logMsgC = liftIO . f}
  where
    f (LogMsg Debug msg) = TinyLog.debug tl $ TinyLog.msg msg
    f (LogMsg Info msg) = TinyLog.info tl $ TinyLog.msg msg
    f (LogMsg Error msg) = TinyLog.err tl $ TinyLog.msg msg


-- test logging service that uses underlying 'Writer' monad to write log messages
testLogging :: Logging (MonadWriter [LogMsg])
testLogging = Logging {logMsgC = W.tell . pure}
