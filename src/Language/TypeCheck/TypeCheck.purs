module Steienr.Language.TypeCheck.TypeCheck where

import Prelude
import Control.Monad.Error.Class (class MonadError)
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Steiner.Control.Monad.Unify (UnifyT, Unknown, fresh, substitute, zonk)
import Steiner.Language.Ast (Expression(..), Literal(..), everywhereOnExpression)
import Steiner.Language.Error (SteinerError, TheImpossibleHappened(..), TypeError(..), failWith, toSteinerError)
import Steiner.Language.Type (SkolemScope(..), Type(..), everywhereOnTypeM, freeTypeVariables, replaceTypeVars, typeFloat, typeInt, typeString)

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

skolemizeTypesInValue :: String -> SkolemScope -> Unknown -> Expression -> Expression
skolemizeTypesInValue ident scope constant = everywhereOnExpression go
  where
  go (TypedExpression checked expr ty) = TypedExpression checked expr $ skolemize ident scope constant ty

  go other = other

-- |
-- Quantify over all free variables in a type
--
quantify :: Type -> Type
quantify ty = foldr (\a b -> TForall a b Nothing) ty $ freeTypeVariables ty

-- | 
-- Remove any ForAlls and ConstrainedType constructors in a type by introducing new unknowns
-- or TypeClassDictionary values.
--
-- This is necessary during type checking to avoid unifying a polymorphic type with a
-- unification variable.
--
instantiatePolyTypeWithUnknowns :: forall m. MonadError SteinerError m => Expression -> Type -> UnifyT Type m (Tuple Expression Type)
instantiatePolyTypeWithUnknowns val (TForall ident ty _) = do
  u <- freshUnknown
  instantiatePolyTypeWithUnknowns val $ replaceTypeVars ident u ty

instantiatePolyTypeWithUnknowns val ty = pure $ Tuple val ty

-- |
-- Find a substitution so 2 types are equal
--
unify :: forall m. MonadError SteinerError m => Type -> Type -> UnifyT Type m Unit
unify (TUnknown name) (TUnknown name')
  | name == name' = pure unit

unify (TUnknown name) ty = substitute unify (toSteinerError $ RecursiveType { ty, varName: "?" <> show name }) name ty

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

unify (TForall ident ty Nothing) _ = failWith $ NoSkolemScope ident ty

unify other forall'@(TForall _ _ _) = forall' `unify` other

unify (Skolem ident constant _) (Skolem ident' constant' _)
  | constant == constant' = pure unit
  | otherwise = failWith $ ident `DifferentSkolemConstants` ident'

unify (TLambda from to) (TLambda from' to') = do
  unify from from'
  unify to to'

unify left right
  | left == right = pure unit
  | otherwise = failWith $ left `CannotUnify` right

-- |
-- Checks if a type is at least as polymorphic as another.
-- This is a wrapper around subsumes' which intercepts unification errors and changes
-- them to more accurate ones 
--
subsumes :: forall m. MonadError SteinerError m => Type -> Type -> UnifyT Type m Unit
subsumes first second = do
  -- TODO: find a way to make this show better error message
  -- newErr = const $ NotPolymorphicEnough first' second'
  -- catchError go \original@(SteinerError { error }) ->
  -- failWith $ onMatch { cannotUnify: newErr, notPolymorphicEnough: newErr } (const original) error
  first' <- zonk first
  second' <- zonk second
  subsumes' first' second'

-- |
-- Internal version of subsumes which doesn't intercept unification errors.
--
subsumes' :: forall m. MonadError SteinerError m => Type -> Type -> UnifyT Type m Unit
subsumes' other (TForall ident ty _) = do
  instantiated <- replaceVarWithUnknown ident ty
  subsumes other instantiated

subsumes' (TForall ident ty Nothing) _ = failWith $ NoSkolemScope ident ty

subsumes' (TForall ident ty (Just scope)) other = do
  constant <- newSkolemConstant
  let
    skolemised = skolemize ident scope constant ty
  subsumes skolemised other

subsumes' (TLambda from to) (TLambda from' to') = do
  subsumes from' from
  subsumes to to'

subsumes' ty ty' = ty `unify` ty'

-- |
-- Infer the type of an expression
--
infer :: forall m. MonadError SteinerError m => Expression -> UnifyT Type m (Tuple Expression Type)
infer expr = failWith $ InvalidInference expr

-- |
-- Check if an expression has a certain type
--
check :: forall m. MonadError SteinerError m => Expression -> Type -> UnifyT Type m Expression
check expression (TForall ident ty _) = do
  scope <- newSkolemScope
  constant <- newSkolemConstant
  let
    skolemisedType = skolemize ident scope constant ty

    skolemisedExpression = skolemizeTypesInValue ident scope constant expression
  expression' <- check skolemisedExpression skolemisedType
  pure $ TypedExpression true expression' (TForall ident ty (Just scope))

check expression unknown@(TUnknown _) = do
  Tuple expression' ty <- infer expression
  -- Don't unify an unknown with an inferred polytype
  Tuple expression'' ty' <- instantiatePolyTypeWithUnknowns (TypedExpression true expression' ty) ty
  unify ty' unknown
  pure $ TypedExpression true expression'' ty'

check v@(Literal (IntLit _)) t
  | t == typeInt = pure $ TypedExpression true v t

check v@(Literal (FloatLit _)) t
  | t == typeFloat = pure $ TypedExpression true v t

check v@(Literal (StringLit _)) t
  | t == typeString = pure $ TypedExpression true v t

check (TypedExpression checked expression ty) other = do
  ty' <- introduceSkolemScopes ty
  subsumes ty' other
  expression' <-
    if not checked then
      check expression ty'
    else
      pure expression
  pure $ TypedExpression true (TypedExpression checked expression' ty') other

check ast ty = failWith $ NeedsType ast ty
