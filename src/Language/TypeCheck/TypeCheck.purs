module Steienr.Language.TypeCheck.TypeCheck where

import Prelude
import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.Tuple (uncurry)
import Data.Variant (onMatch)
import Steiner.Control.Monad.Unify (UnifyT, Unknown, fresh, substitute, zonk)
import Steiner.Language.Error (SteinerError(..), UnificationErrors, cannotUnify, differentSkolemConstants, noSkolemScope, notPolymorphicEnough, recursiveType)
import Steiner.Language.Type (SkolemScope(..), Type(..), everywhereOnTypeM, freeTypeVariables, replaceTypeVars)

-- |
-- Generate an unique unknown
--
freshUnknown :: forall m. Monad m => UnifyT Type m Type
freshUnknown = TUnknown <$> fresh

-- |
-- Genreate an unique skolem scope
--
newSkolemScope :: forall m. Monad m => UnifyT Type m SkolemScope
newSkolemScope = SkolemScope <$> fresh

-- |
-- Generate an unique skolem constant
--
newSkolemConstant :: forall m. Monad m => UnifyT Type m Int
newSkolemConstant = fresh

-- |
-- Add skolem scopes to all Foralls which don't have one
--
introduceSkolemScopes :: forall m. Monad m => Type -> UnifyT Type m Type
introduceSkolemScopes =
  everywhereOnTypeM case _ of
    TForall ident ty Nothing -> do
      scope <- newSkolemScope
      pure $ TForall ident ty $ Just scope
    other -> pure other

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
skolemize ident scope constant = replaceTypeVars ident $ Skolem ident constant scope

-- |
-- Quantify over all free variables in a type
--
quantify :: Type -> Type
quantify ty = foldr (\a b -> TForall a b Nothing) ty $ freeTypeVariables ty

-- |
-- Find a substitution so 2 types are equal
--
unify :: forall m. MonadError UnificationErrors m => Type -> Type -> UnifyT Type m Unit
unify (TUnknown name) (TUnknown name')
  | name == name' = pure unit

unify (TUnknown name) ty = substitute unify (recursiveType { ty, varName: "?" <> show name }) name ty

unify ty s@(TUnknown _) = unify s ty

unify (TForall ident ty (Just scope)) (TForall ident' ty' (Just scope')) = do
  constant <- newSkolemConstant
  let
    skolemised = skolemize ident scope constant ty

    skolemised' = skolemize ident' scope' constant ty'
  skolemised `unify` skolemised'

unify (TForall ident ty (Just scope)) other = do
  constant <- newSkolemConstant
  let
    skolemised = skolemize ident scope constant ty
  skolemised `unify` other

unify (TForall ident ty Nothing) _ = throwError $ noSkolemScope ident ty

unify other forall'@(TForall _ _ _) = forall' `unify` other

unify (Skolem ident constant _) (Skolem ident' constant' _)
  | constant == constant' = pure unit
  | otherwise = throwError $ ident `differentSkolemConstants` ident'

unify (TLambda from to) (TLambda from' to') = do
  unify from from'
  unify to to'

unify left right
  | left == right = pure unit
  | otherwise = throwError $ left `cannotUnify` right

subsumes :: forall m. MonadError UnificationErrors m => Type -> Type -> UnifyT Type m Unit
subsumes first second =
  catchError go \original@(SteinerError { error }) ->
    throwError $ onMatch { cannotUnify: uncurry notPolymorphicEnough } (const original) error
  where
  go = do
    first' <- zonk first
    second' <- zonk second
    subsumes' first' second'

subsumes' :: forall m. MonadError UnificationErrors m => Type -> Type -> UnifyT Type m Unit
subsumes' other (TForall ident ty _) = do
  instantiated <- replaceVarWithUnknown ident ty
  subsumes other instantiated

subsumes' (TForall ident ty Nothing) _ = throwError $ noSkolemScope ident ty

subsumes' (TForall ident ty (Just scope)) other = do
  constant <- newSkolemConstant
  let
    skolemised = skolemize ident scope constant ty
  subsumes skolemised other

subsumes' (TLambda from to) (TLambda from' to') = do
  subsumes from' from
  subsumes to to'

subsumes' ty ty' = ty `unify` ty'
