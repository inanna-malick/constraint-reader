
------------------------------------------------------------------------------
import           Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.State as S
import qualified Control.Monad.Writer as W
import qualified Control.Monad.Reader as R
import qualified Data.IORef as IORef
import           Data.Map (Map)
import qualified Data.Map as Map
------------------------------------------------------------------------------
import           Test.Hspec
------------------------------------------------------------------------------
import           Lib
import           Service.DataStore
import           Service.Logging
import           Service.Metrics
import           Types (Todo(..))
------------------------------------------------------------------------------

data TestEnv = TestEnv
  { testEnvLogging :: Logging (W.MonadWriter [LogMsg])
  , testEnvMetrics :: Metrics (S.MonadState (Map CounterName Int))
  , testEnvDataStore :: DataStore DataStoreError MonadIO
  }

instance HasMetrics TestEnv (Metrics (S.MonadState (Map CounterName Int))) where
    metricsLen = testEnvMetrics

instance HasLogging TestEnv (Logging (W.MonadWriter [LogMsg])) where
    logging = testEnvLogging

instance HasDataStore TestEnv (DataStore DataStoreError MonadIO) where
    datastore = testEnvDataStore


main :: IO ()
main = hspec $ do
  describe "data store read/write logic" $ do
    it "writes todo's to the data store and is able to read back a list of the same todo's" $ do
      let (t1,t2,t3) = ( Todo "todo1" "body1"
                       , Todo "todo2" "body2"
                       , Todo "todo3" "body3"
                       )

      (metrics, logs, todosRead)<- runTest $ do
          Right _ <- writeTodo t1
          Right _ <- writeTodo t2
          Right _ <- writeTodo t3
          readAllTodos


      todosRead `shouldBe` Right [t1,t2,t3]
      (fmap logLevel logs) `shouldNotContain` [Error]
      metrics `shouldBe` Map.fromList [ (CounterName "attempted-list-appends", 3)
                                      , (CounterName "todo-list-appends", 3)
                                      ]

    it "catches profanity violations" $ do
      let t1 = Todo "note to self" "heck is considered a swear word in this context, weird"

      (metrics, logs, res)<- runTest $ do
          writeTodo t1


      res `shouldBe` (Left $ ValidationError "no swearing allowed")
      (fmap logLevel logs) `shouldContain` [Error]
      metrics `shouldBe` Map.fromList [ (CounterName "attempted-list-appends", 1)
                                      , (CounterName "profanity-violations", 1)
                                      ]


    it "logs error on decode fail" $ do
      let garbageBytes = "foobarbaz,binary noise,0XDEADBEEF,etc"

      (metrics, logs, res) <- runTest $ do
          -- actual append operation should succeed
          Right _ <- appendToList id garbageBytes
          -- but attempting to read it back out cause sa decode failure
          readAllTodos


      res `shouldBe` (Left $ DecodeError garbageBytes)
      (fmap logLevel logs) `shouldContain` [Error]
      metrics `shouldBe` Map.fromList [(CounterName "decode-errors", 1)]

withTestEnv :: (TestEnv -> IO a) -> IO a
withTestEnv k = do
  ior <- IORef.newIORef []
  k $ env ior
  where
    env ior = TestEnv
            { testEnvLogging = testLogging
            , testEnvMetrics = testMetrics
            , testEnvDataStore = inMemoryDataStore ior
            }

-- | run test logic in a stack providing an in-memory data store, a logger (backed by writer monad)
-- and a metrics (backed by state monad). After running test logic in monad stack, returns all
-- log messages written and final metric state
runTest
  :: S.StateT (Map CounterName Int)
              (W.WriterT [LogMsg]
                         (R.ReaderT TestEnv IO
                         )
              ) a
  -> IO (Map CounterName Int, [LogMsg], a)
runTest ma = do
      testRes <-
        withTestEnv $ \te -> flip R.runReaderT te $ W.runWriterT $ flip S.runStateT Map.empty $ do
          ma

      let metrics   :: Map CounterName Int
          metrics = snd $ fst testRes
          logs      :: [LogMsg]
          logs = snd testRes
          res = fst $ fst testRes

      pure (metrics, logs, res)
