module Lib
    ( writeTodo
    , readAllTodos
    ) where

------------------------------------------------------------------------------
import qualified Data.Aeson as Aeson
import           Data.List (isInfixOf)
------------------------------------------------------------------------------
import           Service.DataStore
import           Service.Logging
import           Service.Metrics
import           Types (Todo(..))
------------------------------------------------------------------------------

-- TODO:try imposing exceptT constraint via some of these instead of returning in Either?
writeTodo
  :: ( MonadDataStore DataStoreError m
     , MonadLogging m
     , MonadMetrics m
     , Monad m
     )
  => Todo
  -> m (Either DataStoreError ())
writeTodo todo = do
      logMsg $ LogMsg Debug $ "[DATASTORE] attempting to write todo: " ++ show todo
      incrementCounter $ CounterName "attempted-list-appends"

      -- business logic
      if ("heck" `isInfixOf` body todo)
        then do
          logMsg $ LogMsg Error "[DATASTORE] this is a christian server, no swearing!"
          incrementCounter $ CounterName "profanity-violations"
          -- TODO: better name or w/e
          pure $ Left $ ValidationError "no swearing allowed"

        -- if validation passes, append to the ioref's list
        else do
          incrementCounter $ CounterName "todo-list-appends"
          res <- appendToList Aeson.encode todo
          logMsg $ LogMsg Info $ "[DATASTORE] wrote todo: " ++ show todo
          pure res


readAllTodos
  :: ( MonadDataStore DataStoreError m
     , MonadLogging m
     , MonadMetrics m
     , Monad m
     )
  => m (Either DataStoreError [Todo])
readAllTodos = do
  -- TODO: kinda spurious logging maybe
  logMsg $ LogMsg Debug $ "[DATASTORE] attempting to read all todos"
  -- TODO: do logging, metrics, etc here, also move these fn's to Lib to replace biz logic
  -- TODO: have domain error type instead of using IMK stuff errywhere
  res <- readAllList (\x -> maybe (Left $ DecodeError x) Right $ Aeson.decode x)
  case res of
    Left (DecodeError x) -> do
      logMsg $ LogMsg Error $ "unable to decode value" ++ show x
      incrementCounter $ CounterName "decode-errors"
    Left (DataStoreError re) ->
      logMsg $ LogMsg Error $ "unable to connect to data store due to error: " ++ show re
    _ -> pure ()
  pure res
