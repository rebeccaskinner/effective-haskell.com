---
chapter: 13
exercise-id: 1
name: Building Out The Monad Transformer Library
summary: "
Summary TBD
"
---

## Building Out The Monad Transformer Library {.problem}

In this chapter we focused on two specific Monad transformers: `StateT` and
`ExceptT`. The `transformers` library provides several other commonly used Monad
transformers. One of the most common Monad transformers is the `ReaderT`
transformer. This Monad transformer lets you write computations that have a
read-only environment:

```haskell
newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}
```

The two basic operations for a `ReaderT` Monad are `ask`, which fetches the
value from the read-only environment, and`local`, which lets you run a `ReaderT`
action with a modified local read-only environment. Their types are:

```haskell
ask :: Monad m => ReaderT r m r
local :: Monad m => (r -> r) -> ReaderT r m a -> ReaderT r m a
```

In this exercise, write `Functor`, `Applicative`, `Monad`, `MonadIO`, and
`MonadTrans` instances for `ReaderT`, and provide a definition for both `ask`
and `local`. Once you have created a working definition of `ReaderT`, add a new
class called `MonadReader`:

```haskell
class Monad m => MonadReader r m | m -> r where
  ask :: m r
  local :: (r -> r) -> m a -> m a
```

Next, finish writing the following instances:

```haskell
instance Monad m => MonadReader r (Reader.ReaderT r m) where
instance MonadReader r m => MonadReader r (ExceptT e m) where
instance MonadReader r m => MonadReader r (StateT s m) where
```

### Hint 1 {.hint}

Some high level hint text

### Hint 2 {.hint}

Some more detailed hint text

### Hint 3 {.hint}

Even more detailed hint text

### Solution {.solution}

A complete solution for the exercise
