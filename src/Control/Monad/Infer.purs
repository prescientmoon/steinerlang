module Steiner.Control.Monad.Infer where

import Prelude
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, local, runReaderT)
import Control.Monad.State (class MonadState)
import Control.Monad.Writer (class MonadTell, class MonadWriter, WriterT, runWriterT, tell)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Steiner.Control.Monad.Unify (UnifyState(..), UnifyT, runUnifyT)
import Steiner.Language.Type (Type)

-- This is the output accumulated by the infer monad.
-- It contains a set of constraints 
-- and a map of location -> type paris
newtype InferOutput
  = InferOutput
  { constraints :: Array (Tuple Type Type)
  }

-- |
-- Constraint 2 types together
createConstraint :: forall m. MonadTell InferOutput m => Type -> Type -> m Unit
createConstraint from to = tell $ InferOutput { constraints: [ Tuple from to ] }

infixr 5 createConstraint as ~~

derive instance newtypeInferOutput :: Newtype InferOutput _

derive newtype instance semigroupInferOutput :: Semigroup InferOutput

derive newtype instance monoidInferOutput :: Monoid InferOutput

-- |
-- Read only environment for type inference
--
newtype InferEnv
  = InferEnv
  { typeEnv :: Map String Type
  }

-- |
-- Run a computation with a value in scope
--
withScope :: forall m a. MonadReader InferEnv m => String -> Type -> m a -> m a
withScope name ty = local (\(InferEnv { typeEnv }) -> InferEnv { typeEnv: Map.insert name ty typeEnv })

-- |
-- Lookup a variable in the current scope 
--
lookupVar :: forall m. MonadAsk InferEnv m => String -> m (Maybe Type)
lookupVar name = do
  InferEnv { typeEnv } <- ask
  pure $ Map.lookup name typeEnv

derive instance newtypeInferEnv :: Newtype InferEnv _

derive newtype instance semigroupInferEnv :: Semigroup InferEnv

derive newtype instance monoidInferEnv :: Monoid InferEnv

-- Monad for type inference to run in
newtype InferT m a
  = InferT (WriterT InferOutput (ReaderT InferEnv (UnifyT m)) a)

-- |
-- Run a computation in the Infer monad
--
runInferT :: forall m a. InferT m a -> m (Tuple (Tuple a InferOutput) UnifyState)
runInferT (InferT m) = runUnifyT (runReaderT (runWriterT m) mempty) (UnifyState { nextVar: 0 })

-- Typeclass instances for InferT
derive instance functorInferT :: Functor m => Functor (InferT m)

derive newtype instance applyInferT :: Monad m => Apply (InferT m)

derive newtype instance applicativeInferT :: Monad m => Applicative (InferT m)

derive newtype instance bindInferT :: Monad m => Bind (InferT m)

derive newtype instance monadInferT :: Monad m => Monad (InferT m)

derive newtype instance monadTellInferT :: Monad m => MonadTell InferOutput (InferT m)

derive newtype instance monadWritherInferT :: Monad m => MonadWriter InferOutput (InferT m)

derive newtype instance monadStateInferT :: Monad m => MonadState UnifyState (InferT m)

derive newtype instance monadAskInferT :: Monad m => MonadAsk InferEnv (InferT m)

derive newtype instance monadReaderInferT :: Monad m => MonadReader InferEnv (InferT m)
