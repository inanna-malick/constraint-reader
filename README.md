# constraint-reader

minimal example of technique:

```
λ: :set -XKindSignatures -XConstraintKinds -XRankNTypes 
λ: import Data.Constraint
λ: data Logging (mc :: (* -> *) -> Constraint) 
     = Logging { logInfo :: forall m . mc m => String -> m () }

λ: import Control.Monad.IO.Class 
λ: let ioLogging 
     = ( Logging { logInfo = liftIO . putStrLn . ("[info]: " ++) } :: Logging MonadIO )
λ: :t logInfo ioLogging
logInfo ioLogging :: MonadIO m => String -> m ()
λ: logInfo ioLogging "test msg"
[info]: test msg
```
