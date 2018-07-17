module Server (runServer) where

------------------------------------------------------------------------------
import qualified Control.Monad.Except as E
import           Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.Reader as R
-- not great for production (obfuscates actual potentially non-total string conversions)
-- but string-conv is extremely convenient
import           Data.String.Conv (toS)
import qualified Database.Redis.IO as Redis
import           Network.Wai.Handler.Warp (run)
import           Servant
import qualified System.Logger as TinyLog
------------------------------------------------------------------------------
import           Service.DataStore
import           Service.Logging
import           Lib
import           Service.Metrics
import           Types (Todo(..))
------------------------------------------------------------------------------

type API = "todos" :> Get '[JSON] [Todo]
      :<|> "todos" :> ReqBody '[JSON] Todo :> Post '[JSON] ()


runServer :: IO ()
runServer = do
  logger <- TinyLog.new TinyLog.defSettings
  pool <- Redis.mkPool logger Redis.defSettings
  withEnv logger pool (run 8081 . app)

app :: Env -> Application
app = serve api . server


server :: Env -> Server API
server env = hoistServer api (readerToHandler env) serverT


api :: Proxy API
api = Proxy


readerToHandler :: Env -> R.ReaderT Env Handler a -> Handler a
readerToHandler env r = R.runReaderT r env

-- Also I figured out why pure () works in the work codebase, msg nuttycombe re that later
-- it's because api there just does nothing and returns 200, which, AKA, pure ()
serverT :: ServerT API (R.ReaderT Env Handler)
serverT = getTodos
    :<|> addTodo

  where
    addTodo todo = do
      res <- writeTodo todo
      either (R.lift . E.throwError . toErrorResponse) pure res

    getTodos = do
      res <- readAllTodos
      either (R.lift . E.throwError . toErrorResponse) pure res

    toErrorResponse = \case
      ValidationError msg -> err400 { errBody =  toS $ "validation error:" ++ msg }
      DecodeError _ -> err500 { errBody = "error decoding object in database" }
      DataStoreError re ->
        err500 { errBody = toS $ "unable to connect due to data store error: " ++ show re }



-- TOP-LEVEL ENV BOILERPLATE, much of this could be autogen'd
data Env =
  Env { envLogging :: Logging MonadIO
      , envMetrics :: Metrics RedisMetricsDeps
      , envDataStore :: DataStore DataStoreError RedisDataStoreDeps
      }

instance HasMetrics Env (Metrics RedisMetricsDeps) where
    metricsLen = envMetrics

instance HasLogging Env (Logging MonadIO) where
    logging = envLogging

instance HasDataStore Env (DataStore DataStoreError RedisDataStoreDeps) where
    datastore = envDataStore

withEnv
  :: TinyLog.Logger
  -> Redis.Pool
  -> (Env -> IO a)
  -> IO a
withEnv logger pool k = do
  k env
  where
    env = Env { envLogging = tinylogLogging logger
              , envMetrics = redisMetrics pool
              , envDataStore = redisDataStore pool
              }
