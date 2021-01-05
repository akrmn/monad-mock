{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.State.IO.Lazy
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Lazy state monads, passing an updatable state through a computation.
--
-- For a strict version with the same interface, see
-- "Control.Monad.Trans.State.IO.Strict".
-----------------------------------------------------------------------------

module Control.Monad.Trans.State.IO.Lazy (
    -- * The StateT monad transformer
    StateT(..),
    state,
    stateT,
    evalStateT,
    execStateT,
    mapStateT,
    withStateT,
    -- * State operations
    get,
    put,
    modify,
    modify',
    gets,
  ) where

import qualified Control.Monad.State.Class as Class

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader.Class (MonadReader (..))
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Functor.Contravariant (Contravariant (..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef)
import Data.Tuple (swap)

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))
import Control.Monad.Fix (MonadFix (..))

-- ---------------------------------------------------------------------------
-- | Construct a state monad computation from a function.
-- (The inverse of 'runState'.)
state :: MonadIO m
      => (s -> (a, s))  -- ^pure state transformer
      -> StateT s m a   -- ^equivalent state-passing computation
state f = StateT \ref ->
  liftIO $ atomicModifyIORef ref (swap . f)
{-# INLINE state #-}

-- ---------------------------------------------------------------------------
-- | A state transformer monad parameterized by:
--
--   * @s@ - The state.
--
--   * @m@ - The inner monad.
--
-- The 'return' function leaves the state unchanged, while @>>=@ uses
-- the final state of the first computation as the initial state of
-- the second.
newtype StateT s m a = StateT { unStateT :: IORef s -> m a }
  deriving
    ( Functor
    , Contravariant
    , Applicative
    , Alternative
    , Monad
    , MonadFail
    , MonadFix
    , MonadPlus
    , MonadIO
    , MonadError e
    , MonadWriter w
    , MonadCont
    ) via ReaderT (IORef s) m
  deriving
    ( MonadTrans
    ) via ReaderT (IORef s)

instance (MonadReader r m, MonadIO m) => MonadReader r (StateT s m) where
  ask = lift ask
  local = mapStateT . local
  reader = lift . reader

instance MonadIO m => Class.MonadState s (StateT s m) where
  state = state
  get = get
  put = put

runStateT :: MonadIO m => StateT s m a -> s -> m (a, s)
runStateT (StateT m) s = do
  r <- liftIO $ newIORef s
  x <- m r
  s' <- liftIO $ readIORef r
  pure (x, s')

stateT :: MonadIO m => (s -> m (a, s)) -> StateT s m a
stateT f = StateT \ref -> do
  s <- liftIO $ readIORef ref
  (x, s') <- f s
  liftIO $ writeIORef ref s'
  pure x

-- | Evaluate a state computation with the given initial state
-- and return the final value, discarding the final state.
--
-- * @'evalStateT' m s = 'liftM' 'fst' ('runStateT' m s)@
evalStateT :: MonadIO m => StateT s m a -> s -> m a
evalStateT m s = do
    ~(a, _) <- runStateT m s
    return a
{-# INLINE evalStateT #-}

-- | Evaluate a state computation with the given initial state
-- and return the final state, discarding the final value.
--
-- * @'execStateT' m s = 'liftM' 'snd' ('runStateT' m s)@
execStateT :: MonadIO m => StateT s m a -> s -> m s
execStateT m s = do
    ~(_, s') <- runStateT m s
    return s'
{-# INLINE execStateT #-}

-- | Map both the return value and final state of a computation using
-- the given function.
--
-- * @'runStateT' ('mapStateT' f m) = f . 'runStateT' m@
mapStateT :: (MonadIO m, MonadIO n) => (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT f m = stateT $ f . runStateT m
{-# INLINE mapStateT #-}

-- | @'withStateT' f m@ executes action @m@ on a state modified by
-- applying @f@.
--
-- * @'withStateT' f m = 'modify' f >> m@
withStateT :: MonadIO m => (s -> s) -> StateT s m a -> StateT s m a
withStateT f m = stateT $ runStateT m . f
{-# INLINE withStateT #-}

-- | Fetch the current value of the state within the monad.
get :: MonadIO m => StateT s m s
get = state $ \ s -> (s, s)
{-# INLINE get #-}

-- | @'put' s@ sets the state within the monad to @s@.
put :: MonadIO m => s -> StateT s m ()
put s = state $ \ _ -> ((), s)
{-# INLINE put #-}

-- | @'modify' f@ is an action that updates the state to the result of
-- applying @f@ to the current state.
--
-- * @'modify' f = 'get' >>= ('put' . f)@
modify :: MonadIO m => (s -> s) -> StateT s m ()
modify f = state $ \ s -> ((), f s)
{-# INLINE modify #-}

-- | A variant of 'modify' in which the computation is strict in the
-- new state.
--
-- * @'modify'' f = 'get' >>= (('$!') 'put' . f)@
modify' :: MonadIO m => (s -> s) -> StateT s m ()
modify' f = do
    s <- get
    put $! f s
{-# INLINE modify' #-}

-- | Get a specific component of the state, using a projection function
-- supplied.
--
-- * @'gets' f = 'liftM' f 'get'@
gets :: MonadIO m => (s -> a) -> StateT s m a
gets f = state $ \ s -> (f s, s)
{-# INLINE gets #-}
