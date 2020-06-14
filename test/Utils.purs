module Test.Utils where

import Prelude
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Reader (ReaderT)
import Data.Either (Either, either, isLeft)
import Data.Tuple (fst)
import Effect.Exception (Error)
import Steiner.Control.Monad.Check (CheckEnv, runWithUnifyT)
import Steiner.Control.Monad.Unify (class Substituable, UnifyT, runUnifyT)
import Steiner.Language.Type (Type)
import Test.Spec.Assertions (fail, shouldSatisfy)

-- |
-- Run an Unify monad which can fail.
--
runExceptUnify :: forall e a t. Substituable t t => UnifyT t (Except e) a -> Either e a
runExceptUnify = map fst <<< runExcept <<< runUnifyT

-- |
-- Run an Unify monad which can fail and needs to keep track of the stuff in scope.
--
runExceptCheck :: forall e a. UnifyT Type (ReaderT CheckEnv (Except e)) a -> Either e a
runExceptCheck = map fst <<< runExcept <<< runWithUnifyT

-- |
-- Expect an either to be a Left.
--
shouldFail :: forall m a e. MonadError Error m => Show a => Show e => Either e a -> m Unit
shouldFail = flip shouldSatisfy isLeft

-- |
-- Expect an either to be a Right.
--
shouldNotFail :: forall m a. MonadError Error m => Show a => Either String a -> m Unit
shouldNotFail = either fail (const $ pure unit)
