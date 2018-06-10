# constraint-reader


NOTES CIRCA JUNEISH

- link fpcomplete readert or simliar as backgroudn reading
-- 'however, if you haven't read it it is my intent that this be able to stand on its own'.
- use testing complex series of services in pure monad stack as motivation
-- I bet bayhac stuff from last year is online. Watch sandy's thing on eff and crib arguments re: utilty, after all it's still all there
- include short reference to, eg, conduit, leaving type non-concrete allows composition of things that embed the monad stack (and thus require more than lift/liftIO)

- start with tests first, have testing at ever phase (eg really minimal test for logger/metrics)
- use to illustrate how pure monad stack tests look (with caveat noted that these tests are kinda meh)
END NOTES CIRCA JUNEISH

blogpost outline:

## Dependency Management using the ReaderT pattern

- key points
-- link
-- one or two things about limitations of mt stacks in async envs
-- capabilities:

- introduce problem: building up a stack of dependencies to do ReaderT pattern
- introduce 1x service with toy example (just copy examples from ReaderT article w/ citation ofc)


-- link this with a few sentences summary about nondesirability of deep monad transformer stacks for async work
https://www.fpcomplete.com/blog/2017/06/readert-design-pattern


#NOTE: what if I just skip this entirely? I'm not looking forward to writing it
### Simple Services using the ReaderT pattern

basic idea: introduce Logging and Metrics because tbf I'll need them anyway

Note: there's some lens boilerplate around `HasLogging` and `HasMetrics` that you don't really need to understand.

```
data Logging m = 
  Logging { logInfo :: String -> m () }

class HasLogging s a | s -> a where
    logging :: Lens' s a

data Metrics m =
  Metrics { incrementCounterC :: CounterName -> m () }

class HasMetrics s a | s -> a where
    metrics :: Lens' s a
```

To save on space I'm going to hold off on implementations, but the standard impls would likely be in IO. 
Calling code would then do something like this

```
data AppCapbilities = AppCaps { appLogging :: Logging IO, appMetrics :: appMetrics IO }
data AppCapbilities m = AppCaps { appLogging :: Logging m, appMetrics :: appMetrics m}
let myCaps = AppCaps { appLogging = ioLogging, appMetrics }
```


- use that to sketch out some service that does both logging and metrics.

### Bring it all together: problems composing services using the ReaderT pattern

-- concrete plan for this section: this goes up to but not past data store, nopes out after failed attempt to
-- make it require the set of capabilities it runs in to include readers
-- show example of datastore parameterized over (HasMetrics caps, HasLogger caps => ReaderT caps IO) and show infinite type problem
--  upon construction

- question: but what happens when you want to have some service that depends on that? Can't reify monad stack
-- everything usually has to be in IO, can't get rid of monadIO constraint, bad test experience.
-- problem: everything that has dependencies on more than one service needs to have a record type with all deps
            (one dep just means readerT on that thing itself, trivial case, but can't just tuple them up..)
-- rephrased problem: this pattern requires you to reify the monad stack each component can run in, so you
                      need to do a bunch of plumbing work to get them all running together (often dropping back down to IO)

## Solution: Constrained ReaderT Pattern

-- solution: express monad stack requirements at each stage as a set of _constraints_ instead of a concrete stack

### Sidebar: Let's learn just enough about Data.Constraint to cargo cult our way through this

(constraints)[https://hackage.haskell.org/package/constraints], a package published by E. Kmett, has this description:

> GHC 7.4 gave us the ability to talk about ConstraintKinds. They stopped crashing the compiler in GHC 7.6.

>This package provides a vocabulary for working with them.


So, that's mildly indimidating. Fortuanately all we really need is one thing, `Constraint`. `Constraint` exists
at the kind level and is used to represent _constraints_ as imposed by type classes. Lets take a look at the kind
of a few common types and constraints to illustrate how these work. `:k` can be used to find the kind of some 
type or constraint from within the repl. Here are some examples:

```
λ: :k Show
Show :: * -> Constraint
λ: :k Int
Int :: *
λ: :k Show Int
Show Int :: Constraint
```

In the above you can see how `Show` is a sort of kind-level function that takes some type of kind `*` and
returns a constraint of kind `Constraint`. By applying the argument `Int` (of kind `*`) to that function
results in a constraint of kind `Constraint`.

```
λ: :k Maybe
Maybe :: * -> *
λ: :k Monad
Monad :: (* -> *) -> Constraint
λ: :k Maybe
Maybe :: * -> *
λ: :k Monad Maybe
Monad Maybe :: Constraint
```

`Monad` is also a kind-level function, like `Show`, but it requires some type of kind `* -> *`, in this
case `Maybe` (which is of kind `* -> *`).

So: we have kind level functions that represent constraints. This is functional programming, so what if
we pass those functions around without fully evaluating them to concrete constraints on some specific type?
Here's what I mean by that:

```
λ: :set -XKindSignatures -XConstraintKinds -XRankNTypes 
λ: import Data.Constraint (Constraint)
λ: data Logging (mc :: (* -> *) -> Constraint) 
     = Logging { logInfo :: forall m . mc m => String -> m () }
```

Here is a record, `Logging`, that is parameterized over a _constraint_ instead of a concrete monad or monad
transformer stack. Lets create a few implementations of it for use in tests first:

```
λ: let ignoreLogging = Logging { logInfo = const $ pure () } :: Logging Monad
```

First we have `ignoreLogging`, for cases where we don't really want any log output. It doesn't 
even really need a monad at all, all it cares about is that it is run on something that is at the
very least an instance of `Applicative` so it ican use `pure` to construct the return value of
`pure ()`. Every `Monad` is also an `Applicative` so this constraint is easy to meet. We can 
run `logInfo` as provided by `ignoreLogging` in `IO` or even in the `Identity` monad.

``` 
λ: logInfo ignoreLogging "test"
λ: runIdentity $ logInfo ignoreLogging "test"
()
```

Nothing happened, just like it says on the box. For test cases where we do care about log messages,
let's construct an implementation that just use a `Writer` to capture them.

```
λ: let writerLogging = Logging { logInfo = tell } :: Logging (MonadWriter [String])
λ: runWriterT $ do traverse (logInfo writerLogging . show) [1..10]; pure ()
((),["1","2","3","4","5","6","7","8","9","10"])
```

Here the `logInfo` function as provided by `writerLogging` writes messages using the provided 
`MonadWriter` instance.

Let's create a more useful implementation now, something that actually logs to standard out:

```
λ: import Control.Monad.IO.Class (MonadIO(..))
λ: let stdoutLogging 
    = Logging { logInfo = liftIO . putStrLn . ("[info]: " ++) } :: Logging MonadIO
λ: :t logInfo stdoutLogging
logInfo stdoutLogging :: MonadIO m => String -> m ()
λ: logInfo stdoutLogging "test msg"
[info]: test msg
```

### Composing services using the Constrained ReaderT Pattern

-- at this point it becomes neccessary to add the MonadLogging, etc type classes 
--- (todo: explore putting the caps constraint right in there)
--- ehh, maybe worse? IDK

-- then show complete example of it with links to boilerplate/disclaimer re omitted same (so no need to repeat boilerplate for each svc)

-- this should take us up through DataStore

### Bringing it all together: one simple app

