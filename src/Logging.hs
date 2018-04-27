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
import           Control.Monad.Trans.Class (lift)
import           Data.Constraint
import           Data.Foldable (traverse_)
import qualified Data.IORef as IORef
import           Data.List (isInfixOf)
import           Control.Monad.Reader

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
