module Steiner.Control.Monad.Unify where

import Prelude
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (class MonadError)
import Control.Monad.State (class MonadState, StateT, gets, modify_, runStateT)
import Data.Lens (Lens', over, view)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple)
import Steiner.Data.Lens (newtypeIso)

-- |
-- Curently unknowns are just ints 
--
type Unknown
  = Int

-- |
-- State for unification
--
newtype UnifyState t
  = UnifyState
  { nextVar :: Unknown
  , currentSubstitution :: Substitution t
  }

-- Lenses
_nextVar :: forall t. Lens' (UnifyState t) Unknown
_nextVar = newtypeIso <<< prop (SProxy :: SProxy "nextVar")

_currentSubstitution :: forall t. Lens' (UnifyState t) (Substitution t)
_currentSubstitution = newtypeIso <<< prop (SProxy :: SProxy "currentSubstitution")

-- Typeclass instances
derive instance newtypeUnifyState :: Newtype (UnifyState t) _

instance semigroupUnifyState :: Substituable t t => Semigroup (UnifyState t) where
  append (UnifyState state) (UnifyState state') =
    UnifyState
      { nextVar: max state.nextVar state'.nextVar
      , currentSubstitution: state.currentSubstitution <> state'.currentSubstitution
      }

instance monoidUnifyState :: Substituable t t => Monoid (UnifyState t) where
  mempty = UnifyState { nextVar: 0, currentSubstitution: mempty }

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
  Incomplete t <= Unifiable t m where
  unify :: t -> t -> UnifyT t m (Substitution t)

infixr 5 unify as ~

type UnificationError t
  = ()

-- |
-- The type checking monad, which provides the state of the type checker, and error reporting capabilities
--
newtype UnifyT t m a
  = UnifyT (StateT (UnifyState t) m a)

-- |
-- Run a computation in the Unify monad
--
runUnifyT :: forall t m a. Substituable t t => UnifyT t m a -> m (Tuple a (UnifyState t))
runUnifyT (UnifyT m) = runStateT m mempty

-- |
-- Generate a fresh unknown
--
fresh :: forall t m. MonadState (UnifyState t) m => m Int
fresh = do
  nextVar <- gets $ view _nextVar
  modify_ $ over _nextVar (1 + _)
  pure nextVar

zonk :: forall t m. Substituable t t => MonadState (UnifyState t) m => t -> m t
zonk ty = do
  sub <- gets $ view _currentSubstitution
  pure $ sub ?= ty

-- |
-- Check if a type contains references to itself
--
occursCheck :: forall m t e. MonadError e m => Incomplete t => e -> Unknown -> t -> UnifyT t m Unit
occursCheck error u t = case isUnknown t of
  Nothing
    | u `Set.member` unknowns t -> throwError error
  _ -> pure unit

-- |
-- Substitute a single unification variable
--
substituteOne :: forall t. Incomplete t => Unknown -> t -> Substitution t
substituteOne u t = Substitution $ Map.singleton u t

-- |
-- Replace a unification variable with the specified value in the current substitution
-- This takes a generic unify function because using the one from the Unifiable typeclass
-- makes avoiding depdendency cycles a lot harder.
--
substitute :: forall e m t. MonadError e m => Incomplete t => (t -> t -> UnifyT t m Unit) -> e -> Unknown -> t -> UnifyT t m Unit
substitute unify error u t' = do
  sub <- gets $ view _currentSubstitution
  let
    t = sub ?= t'
  occursCheck error u t
  let
    current = sub ?= unknown u
  case isUnknown current of
    Just u1
      | u1 == u -> pure unit
    _ -> current `unify` t
  modify_ $ over _currentSubstitution (substituteOne u t <> _)

-- Typeclass instances for UnifyT
derive instance functorUnifyT :: Functor m => Functor (UnifyT t m)

derive newtype instance applyUnifyT :: Monad m => Apply (UnifyT t m)

derive newtype instance applicativeUnifyT :: Monad m => Applicative (UnifyT t m)

derive newtype instance bindUnifyT :: Monad m => Bind (UnifyT t m)

derive newtype instance monadUnifyT :: Monad m => Monad (UnifyT t m)

derive newtype instance monadStateUnifyT :: Monad m => MonadState (UnifyState t) (UnifyT t m)

derive newtype instance monadErrorUnifyT :: (Monad m, MonadError e m) => MonadError e (UnifyT t m)

derive newtype instance monadThrowUnifyT :: (Monad m, MonadThrow e m) => MonadThrow e (UnifyT t m)
