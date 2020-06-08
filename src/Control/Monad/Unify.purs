module Steiner.Control.Monad.Unify where

import Prelude
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
  = Substitution (Map Int t)

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
  unify :: t -> t -> UnifyT m Unit

infixr 5 unify as ~

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
