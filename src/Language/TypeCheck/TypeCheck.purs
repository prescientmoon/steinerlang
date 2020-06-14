module Steienr.Language.TypeCheck.TypeCheck where

import Prelude
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (class MonadReader)
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Steiner.Control.Monad.Check (CheckEnv, lookupVar, withVariable)
import Steiner.Control.Monad.Unify (UnifyT, Unknown, fresh, substitute, zonk)
import Steiner.Language.Ast (Expression(..), Literal(..), everywhereOnExpression)
import Steiner.Language.Error (SteinerError, TheImpossibleHappened(..), TypeError(..), failWith, toSteinerError)
import Steiner.Language.Type (SkolemScope(..), Type(..), everywhereOnTypeM, freeTypeVariables, replaceTypeVars, typeBoolean, typeFloat, typeInt, typeString)

-- |
-- Basically holds the same info as a TypedExpression but we are sure it can't be anything else.
--
data Typed
  = Typed Boolean Expression Type

-- |
-- Transform a Typed value into an expression
--
typedToExpression :: Typed -> Expression
typedToExpression (Typed a b c) = TypedExpression a b c

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
infer :: forall m. MonadError SteinerError m => MonadReader CheckEnv m => Expression -> UnifyT Type m Typed
infer expression@(Literal (IntLit _)) = pure $ Typed true expression typeInt

infer expression@(Literal (FloatLit _)) = pure $ Typed true expression typeFloat

infer expression@(Literal (StringLit _)) = pure $ Typed true expression typeString

infer (Lambda arg body) = do
  typeArg <- freshUnknown
  withVariable arg typeArg do
    body'@(Typed _ _ typeBody) <- infer body
    Tuple body'' typeBody' <- instantiatePolyTypeWithUnknowns (typedToExpression body') typeBody
    pure $ Typed true (Lambda arg body'') $ TLambda typeArg typeBody'

infer (Variable name) = do
  maybeTy <- lookupVar name
  case maybeTy of
    Nothing -> failWith $ NotInScope name
    Just ty -> do
      ty' <- introduceSkolemScopes ty
      pure $ Typed true (Variable name) ty

infer (Application function arg) = do
  function'@(Typed _ _ typeFunction) <- infer function
  Typed _ app typeReturn <- checkApplication (typedToExpression function') typeFunction arg
  pure $ Typed true app typeReturn

infer (Let name value body) = do
  value'@(Typed _ _ typeValue) <- infer value
  typeGeneralized <- quantify <$> zonk typeValue
  body'@(Typed _ _ typeBody) <- withVariable name typeGeneralized $ infer body
  pure $ Typed true (Let name (typedToExpression value') (typedToExpression body')) typeBody

infer expr = failWith $ InvalidInference expr

-- |
-- Check if an expression has a certain type and return an expression instead of a Typed
--
checkToExpression :: forall m. MonadError SteinerError m => MonadReader CheckEnv m => Expression -> Type -> UnifyT Type m Expression
checkToExpression expr ty = typedToExpression <$> check expr ty

-- |
-- Check if an expression has a certain type
--
check :: forall m. MonadError SteinerError m => MonadReader CheckEnv m => Expression -> Type -> UnifyT Type m Typed
check expression (TForall ident ty _) = do
  scope <- newSkolemScope
  constant <- newSkolemConstant
  let
    skolemisedType = skolemize ident scope constant ty

    skolemisedExpression = skolemizeTypesInValue ident scope constant expression
  expression' <- check skolemisedExpression skolemisedType
  pure $ Typed true (typedToExpression expression') (TForall ident ty (Just scope))

check expression unknown@(TUnknown _) = do
  Typed _ expression' ty <- infer expression
  -- Don't unify an unknown with an inferred polytype
  Tuple expression'' ty' <- instantiatePolyTypeWithUnknowns (TypedExpression true expression' ty) ty
  unify ty' unknown
  pure $ Typed true expression'' ty'

check v@(Literal (IntLit _)) t
  | t == typeInt = pure $ Typed true v t

check v@(Literal (FloatLit _)) t
  | t == typeFloat = pure $ Typed true v t

check v@(Literal (StringLit _)) t
  | t == typeString = pure $ Typed true v t

check (Lambda arg body) (TLambda from to) = do
  ret <- withVariable arg from $ check body to
  pure $ Typed true (Lambda arg (typedToExpression ret)) (TLambda from to)

check expression@(Variable name) ty = do
  maybeVar <- lookupVar name
  case maybeVar of
    Nothing -> failWith $ TypedNotInScope name ty
    Just var -> do
      skolemisedVar <- introduceSkolemScopes var
      subsumes ty skolemisedVar
      pure $ Typed true expression ty

check (If condition then' else') ty = do
  condition' <- checkToExpression condition typeBoolean
  then'' <- checkToExpression then' ty
  else'' <- checkToExpression else' ty
  pure $ Typed true (If condition' then'' else'') ty

check (TypedExpression checked expression ty) other = do
  ty' <- introduceSkolemScopes ty
  subsumes other ty'
  expression' <-
    if not checked then
      checkToExpression expression ty'
    else
      pure expression
  pure $ Typed true (TypedExpression checked expression' ty') other

check (Application fn arg) ty = do
  fn'@(Typed _ _ tyFunc) <- infer fn
  Typed _ app tyReturn <- checkApplication (typedToExpression fn') tyFunc arg
  subsumes ty tyReturn
  pure $ Typed true app ty

check expression ty = do
  Typed _ expression' inferredTyped <- infer expression
  inferredTyped `subsumes` ty
  pure $ Typed true expression ty

-- |
-- Check a functional application retsults in a certain type
--
checkApplication :: forall m. MonadError SteinerError m => MonadReader CheckEnv m => Expression -> Type -> Expression -> UnifyT Type m Typed
checkApplication fn ty arg = do
  ty' <- zonk ty
  checkApplication' fn ty' arg

-- |
-- Internal version of checkApplication which doesn't zonk the types at the start 
--
checkApplication' :: forall m. MonadError SteinerError m => MonadReader CheckEnv m => Expression -> Type -> Expression -> UnifyT Type m Typed
checkApplication' fn (TLambda from to) arg = do
  arg' <- typedToExpression <$> check arg from
  pure $ Typed true (Application fn arg') to

checkApplication' fn (TForall ident ty scope) arg = do
  tyUnknown <- freshUnknown
  let
    replaced = replaceTypeVars ident tyUnknown ty
  checkApplication fn replaced arg

checkApplication' fn ty arg = do
  tv@(Typed _ _ ty') <- do
    Typed _ arg' ty' <- infer arg
    Tuple arg'' ty'' <- instantiatePolyTypeWithUnknowns arg' ty'
    pure $ Typed true arg'' ty''
  return <- freshUnknown
  unify ty (ty `TLambda` return)
  pure $ Typed true (Application fn $ typedToExpression tv) return
