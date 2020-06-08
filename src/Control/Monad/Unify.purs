module Steiner.Control.Monad.Unify where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (class MonadError)
import Control.Monad.State (class MonadState, StateT, get, modify_, runStateT)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Tuple (Tuple)

type Unknown
  = Int

-- |
-- State for unification
newtype UnifyState
  = UnifyState
  { nextVar :: Unknown
  }

-- |
-- A substitution maintains a mapping from unification variables to their values
--
newtype Substitution t
  = Substitution (Map Unknown t)

instance semigroupSubstitution :: Substituable t t => Semigroup (Substitution t) where
  append s1@(Substitution m1) (Substitution m2) =
    let
      m12 = (applySubstitution s1) <$> m2
    in
      Substitution $ m12 <> m1

derive newtype instance monoidSubstitution :: Monoid (Substitution t)

-- |
-- A type which supports applying a substitution
--
class Substituable t u where
  applySubstitution :: Substitution t -> u -> u

infixr 6 applySubstitution as ?=

-- |
-- A type which can contain unification variables
--
class
  Substituable t t <= Incomplete t where
  unknown :: Unknown -> t
  isUnknown :: t -> Maybe Unknown
  unknowns :: t -> Set Unknown

-- |
-- A type which supports unification 
--
class
  Incomplete t <= Unifiable m t where
  unify :: t -> t -> UnifyT m (Substitution t)

infixr 5 unify as ~

type UnificationError t
  = ()

-- |
-- The type checking monad, which provides the state of the type checker, and error reporting capabilities
--
newtype UnifyT m a
  = UnifyT (StateT UnifyState m a)

-- |
-- Run a computation in the Unify monad
--
runUnifyT :: forall m a. UnifyT m a -> UnifyState -> m (Tuple a UnifyState)
runUnifyT (UnifyT m) state = runStateT m state

-- |
-- Generate a fresh unknown
--
fresh :: forall t m. Incomplete t => MonadState UnifyState m => m t
fresh = do
  UnifyState { nextVar } <- get
  modify_ $ \(UnifyState { nextVar: currentVar }) -> UnifyState { nextVar: currentVar + 1 }
  pure $ unknown nextVar

-- Typeclass instances for UnifyT
derive instance functorUnifyT :: Functor m => Functor (UnifyT m)

derive newtype instance applyUnifyT :: Monad m => Apply (UnifyT m)

derive newtype instance applicativeUnifyT :: Monad m => Applicative (UnifyT m)

derive newtype instance bindUnifyT :: Monad m => Bind (UnifyT m)

derive newtype instance monadUnifyT :: Monad m => Monad (UnifyT m)

derive newtype instance monadStateUnifyT :: Monad m => MonadState UnifyState (UnifyT m)

derive newtype instance monadErrorUnifyT :: (Monad m, MonadError e m) => MonadError e (UnifyT m)

derive newtype instance monadThrowUnifyT :: (Monad m, MonadThrow e m) => MonadThrow e (UnifyT m)
