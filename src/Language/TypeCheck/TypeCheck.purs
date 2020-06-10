module Steienr.Language.TypeCheck.TypeCheck where

import Prelude
import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Steiner.Control.Monad.Unify (UnifyT, Unknown, fresh, substitute)
import Steiner.Language.Error (UnificationErrors, cannotUnify, recursiveType)
import Steiner.Language.Type (SkolemScope, Type(..), freeTypeVariables, replaceTypeVars)

-- |
-- Generate an unique unknown
--
freshUnknown :: forall m. Monad m => UnifyT Type m Type
freshUnknown = TUnknown <$> fresh

-- |
-- Replace a single type variable with a new unification variable
--
replaceVarWithUnknown :: forall m. Monad m => String -> Type -> UnifyT Type m Type
replaceVarWithUnknown ident ty = do
  var <- freshUnknown
  pure $ replaceTypeVars ident var ty

-- |
-- Remove any foralls in a type by introducing new unknowns.
--
-- This is necessary during type checking to avoid unifying a polymorphic type with a
-- unification variable.
--
instantiate :: forall m. Monad m => Type -> UnifyT Type m Type
instantiate (TForall ident ty scope) = do
  ty' <- replaceVarWithUnknown ident ty
  instantiate ty'

instantiate ty = pure ty

-- | 
-- Skolemize a type variable by replacing its instances with fresh skolem constants
-- 
skolemize :: String -> SkolemScope -> Unknown -> Type -> Type
skolemize ident scope = replaceTypeVars ident <<< Skolem ident scope

-- |
-- Quantify over all free variables in a type
--
quantify :: Type -> Type
quantify ty = foldr (\a b -> TForall a b Nothing) ty $ freeTypeVariables ty

-- |
-- Find a substitution so 2 types are equal
--
unify :: forall m. MonadError UnificationErrors m => Type -> Type -> UnifyT Type m Unit
unify (TUnknown name) ty = substitute unify (recursiveType { ty, varName: "?" <> show name }) name ty

unify ty s@(TUnknown _) = unify s ty

unify (TLambda from to) (TLambda from' to') = do
  unify from from'
  unify to to'

unify left right
  | left == right = pure mempty
  | otherwise = throwError $ left `cannotUnify` right
