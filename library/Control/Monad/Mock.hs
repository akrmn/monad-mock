{-# LANGUAGE CPP #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
This module provides a monad transformer that helps create “mocks” of
@mtl@-style typeclasses, intended for use in unit tests. A mock can be
executed by providing a sequence of expected monadic calls and their results,
and the mock will verify that the computation conforms to the expectation.

For example, imagine a @MonadFileSystem@ typeclass, which describes a class of
monads that may perform filesystem operations:

@
class 'Monad' m => MonadFileSystem m where
  readFile :: 'FilePath' -> m 'String'
  writeFile :: 'FilePath' -> 'String' -> m ()
@

Using 'MockT', it’s possible to test computations that use @MonadFileSystem@
in a completely pure way:

@
copyFile :: MonadFileSystem m => 'FilePath' -> 'FilePath' -> m ()
copyFile a b = do
  x <- readFile a
  writeFile b x

spec = describe "copyFile" '$'
  it "reads a file and writes its contents to another file" '$'
    'Control.Exception.evaluate' '$' copyFile "foo.txt" "bar.txt"
      'Data.Function.&' 'runMock' [ ReadFile "foo.txt" ':->' "contents"
                , WriteFile "bar.txt" "contents" ':->' () ]
@

To make the above code work, all you have to do is write a small GADT that
represents typeclass method calls and implement the 'Action' typeclass:

@
data FileSystemAction r where
  ReadFile :: 'FilePath' -> FileSystemAction 'String'
  WriteFile :: 'FilePath' -> 'String' -> FileSystemAction ()
deriving instance 'Eq' (FileSystemAction r)
deriving instance 'Show' (FileSystemAction r)

instance 'Action' FileSystemAction where
  'eqAction' (ReadFile a) (ReadFile b)
    = if a '==' b then 'Just' 'Refl' else 'Nothing'
  'eqAction' (WriteFile a b) (WriteFile c d)
    = if a '==' c && b '==' d then 'Just' 'Refl' else 'Nothing'
  'eqAction' _ _ = 'Nothing'
@

Then, just write a @MonadFileSystem@ instance for 'MockT':

@
instance 'Monad' m => MonadFileSystem ('MockT' FileSystemAction m) where
  readFile a = 'mockAction' "readFile" (ReadFile a)
  writeFile a b = 'mockAction' "writeFile" (WriteFile a b)
@

For some Template Haskell functions that eliminate the need to write the above
boilerplate, look at 'Control.Monad.Mock.TH.makeAction' from
"Control.Monad.Mock.TH".
-}
module Control.Monad.Mock
  ( -- * The MockT monad transformer
    MockT
  , Mock
  , runMockT
  , runMock
  , mockAction

  -- * Actions and actions with results
  , Action(..)
  , ShowAction(..)
  , WithResult(..)
  ) where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadThrow, MonadMask)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (StateT, MonadState(..), runStateT)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Trans.Control (ComposeSt, MonadBaseControl(..), MonadTransControl(..), defaultLiftBaseWith, defaultLiftWith, defaultRestoreM, defaultRestoreT)
import Control.Monad.Writer (MonadWriter)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Type.Equality ((:~:)(..))

error' :: String -> a
#if MIN_VERSION_base(4,9,0)
error' = errorWithoutStackTrace
#else
error' = error
#endif

-- | This class is used for converting an 'Action' to a 'String'. There's an
-- overlappable instance that uses 'Show', which works for any type @f@ with an
-- instance @forall a. 'Show' (f a)@. That, in turn, can be derived by
-- GHC using a standalone @deriving@ clause.
-- Alternatively, you can define your own @ShowAction@ instance, and it will be
-- used instead of the overlappable one.
class ShowAction f where
  -- | Converts an 'Action' to a 'String', which will be used when displaying
  -- mock failures.
  showAction :: f a -> String

instance {-# OVERLAPPABLE #-} (forall x. Show (f x)) => ShowAction f where
  showAction =  show

-- | A class of types that represent typeclass method calls. The type must be of
-- kind @* -> *@, and its type parameter should represent type of the method’s
-- return type.
class ShowAction f => Action f where
  -- | Compares two 'Action's for equality, and produces a witness of type
  -- equality if the two actions are, in fact, equal.
  eqAction :: f a -> f b -> Maybe (a :~: b)

-- | Represents both an expected call (an 'Action') and its expected result.
data WithResult f where
  (:->) :: f r -> r -> WithResult f

-- | A monad transformer for creating mock instances of typeclasses. In @'MockT'
-- f m a@, @f@ should be an 'Action', which should be a GADT that represents a
-- reified version of typeclass method calls.
newtype MockT f m a = MockT (StateT [WithResult f] m a)
  deriving ( Functor, Applicative, Monad, MonadTrans, MonadIO, MonadBase b
           , MonadReader r, MonadCont, MonadError e, MonadWriter w
           , MonadCatch, MonadThrow, MonadMask )

instance MonadState s m => MonadState s (MockT f m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadTransControl (MockT f) where
  type StT (MockT f) a = StT (StateT [WithResult f]) a
  liftWith = defaultLiftWith MockT (\(MockT x) -> x)
  restoreT = defaultRestoreT MockT

instance MonadBaseControl b m => MonadBaseControl b (MockT f m) where
  type StM (MockT f m) a = ComposeSt (MockT f) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

type Mock f = MockT f Identity

-- | Runs a 'MockT' computation given an expected list of calls and results. If
-- any method is called during the extent of the computation that is unexpected,
-- an exception will be thrown. Additionally, if the computation terminates
-- without making /all/ of the expected calls, an exception is raised.
runMockT :: forall f m a. (Action f, Monad m) => [WithResult f] -> MockT f m a -> m a
runMockT actions (MockT x) = runStateT x actions >>= \case
  (r, []) -> return r
  (_, remainingActions) -> error'
     $ "runMockT: expected the following unexecuted actions to be run:\n"
    ++ unlines (map (\(action :-> _) -> "  " ++ showAction action) remainingActions)

runMock :: forall f a. Action f => [WithResult f] -> Mock f a -> a
runMock actions x = runIdentity $ runMockT actions x

-- | Logs a method call within a mock.
mockAction :: (Action f, Monad m) => String -> f r -> MockT f m r
mockAction fnName action = MockT $ get >>= \case
  [] -> error'
     $ "runMockT: expected end of program, called " ++ fnName ++ "\n"
    ++ "  given action: " ++ showAction action ++ "\n"
  (action' :-> r) : actions
    | Just Refl <- action `eqAction` action' -> put actions >> return r
    | otherwise -> error'
         $ "runMockT: argument mismatch in " ++ fnName ++ "\n"
        ++ "  given: " ++ showAction action ++ "\n"
        ++ "  expected: " ++ showAction action' ++ "\n"
