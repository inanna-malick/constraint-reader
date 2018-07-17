module Service.Metrics where

------------------------------------------------------------------------------
import           Control.Monad.Reader
import           Control.Monad.State (MonadState)
import qualified Control.Monad.State as S
import           Data.Constraint (Constraint)
import qualified Data.Map as M
import           Data.String.Conv (toS)
import qualified Database.Redis.IO as Redis
------------------------------------------------------------------------------
import           Service.Logging
------------------------------------------------------------------------------

-- METRICS BOILERPLATE

data Metrics (mc :: (* -> *) -> Constraint) =
  Metrics { incrementCounterC :: forall m . mc m => CounterName -> m () }

class HasMetrics caps a | caps -> a where
    -- TODO new name
    metricsLen :: caps -> a

-- | simplified metrics service
class MonadMetrics m where
  incrementCounter :: CounterName -> m ()


instance (HasMetrics env (Metrics mc), MonadReader env m, mc m) => MonadMetrics m where
  incrementCounter cn = do
    fn <- asks (incrementCounterC . metricsLen)
    fn cn

-- METRICS SERVICE IMPL



class ( MonadLogging m
      , MonadIO m
      ) => RedisMetricsDeps m
instance ( MonadLogging m
         , MonadIO m
         ) => RedisMetricsDeps m

-- TODO: impl ability to run redis cmds as a service?
redisMetrics :: Redis.Pool -> Metrics RedisMetricsDeps
redisMetrics pool = Metrics
  { incrementCounterC = \cn -> do
      newValue <- Redis.runRedis pool
        . Redis.commands
        $ Redis.hincrby k (toField cn) 1
      logMsg $ LogMsg Debug $
        "incremented metric:" ++ show cn ++ ", value is now: " ++ show newValue
  }
  where
    toField :: CounterName -> Redis.Field
    toField = toS . unCounterName

    k :: Redis.Key
    k = "todo-app-metrics"

-- test metrics service that uses underlying 'State' monad to store map of metrics
testMetrics :: Metrics (MonadState (M.Map CounterName Int))
testMetrics = Metrics
  { incrementCounterC = \cn -> S.modify (M.insertWith (+) cn 1)
  }

-- TYPES
newtype CounterName = CounterName { unCounterName :: String } deriving (Eq, Ord, Show)