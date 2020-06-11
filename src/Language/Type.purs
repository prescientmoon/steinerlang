module Steiner.Language.Type where

import Prelude
import Data.Identity (Identity(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Steiner.Control.Monad.Unify (class Incomplete, class Substituable, Substitution(..), Unknown, unknowns, (?=))

-- |
-- Scope for skolem variables
--
newtype SkolemScope
  = SkolemScope Unknown

derive instance eqSkolemScope :: Eq SkolemScope

-- |
-- The name of a variable
--
type Name
  = String

-- |
-- Types for the Steiner type system
--
data Type
  -- Types we don't know the value of
  = TUnknown Unknown
  -- Lambda types (will be converted to TConstant later)
  | TLambda Type Type
  -- Opaque types like Int or String
  | TConstant String
  -- Forall types
  | TForall String Type (Maybe SkolemScope)
  -- Type variables 
  | TVariable Name
  -- Skolem variables 
  | Skolem Name Int SkolemScope

derive instance eqType :: Eq Type

-- |
-- Check if a type is a function skiping foralls. 
--
needsParenthesis :: Type -> Boolean
needsParenthesis (TLambda _ _) = true

needsParenthesis (TForall _ _ _) = true

needsParenthesis _ = false

-- Types for literals
typeInt :: Type
typeInt = TConstant "Int"

typeFloat :: Type
typeFloat = TConstant "Float"

typeString :: Type
typeString = TConstant "String"

typeBoolean :: Type
typeBoolean = TConstant "Boolean"

-- |
-- Get all the free type variables in a type
--
freeTypeVariables :: Type -> Set Name
freeTypeVariables = go mempty
  where
  go bound (TVariable name)
    | not $ name `Set.member` bound = Set.singleton name

  go bound (TLambda from to) = go bound from <> go bound to

  go bound (TForall var ty _) = go (Set.insert var bound) ty

  go _ _ = mempty

-- |
-- Get all the Forall bound variables in a type
--
binders :: Type -> Set String
binders (TForall name body _) = Set.insert name $ binders body

binders (TLambda from to) = binders from <> binders to

binders _ = mempty

-- |
-- Run a function on each level of a type and merge the results
--
everythingOnTypes :: forall r. (r -> r -> r) -> (Type -> r) -> Type -> r
everythingOnTypes merge f = go
  where
  go t@(TLambda t1 t2) = f t `merge` go t1 `merge` go t2

  go t@(TForall _ ty _) = f t `merge` go ty

  go other = f other

-- |
-- Map every layer of a type to a monad of another type.
--
everywhereOnTypeM :: forall m. Monad m => (Type -> m Type) -> Type -> m Type
everywhereOnTypeM f = go'
  where
  go continue (TLambda from to) = do
    from' <- continue from
    to' <- continue to
    pure $ TLambda from' to'

  go continue (TForall ident ty sco) = flip (TForall ident) sco <$> continue ty

  go continue other = pure other

  go' ty = do
    ty' <- f ty
    go go' ty'

-- |
-- Map each layer of a type
--
everywhereOnType :: (Type -> Type) -> Type -> Type
everywhereOnType f = unwrap <<< everywhereOnTypeM (Identity <<< f)

-- |
-- Collect all type variables appearing in a type
--
usedTypeVariables :: Type -> Set String
usedTypeVariables = everythingOnTypes (<>) go
  where
  go (TVariable v) = Set.singleton v

  go _ = mempty

-- |
-- Replace a type variable, taking into account variable shadowing
--
replaceTypeVars :: String -> Type -> Type -> Type
replaceTypeVars = replaceTypeVars' mempty
  where
  -- |
  -- Generate a name which is not used for a variable
  --
  genName :: String -> Set String -> String
  genName original inUse = try 0
    where
    try n
      | (original <> show n) `Set.member` inUse = try $ n + 1
      | otherwise = original <> show n

  replaceTypeVars' bound name replacement = go bound
    where
    go :: Set String -> Type -> Type
    go _ (TVariable name')
      | name' == name = replacement

    go bounds (TLambda t1 t2) = TLambda (go bounds t1) (go bounds t2)

    go bounds rho@(TForall var ty scope)
      | var == name = rho
      | var `Set.member` usedTypeVariables replacement =
        let
          inUse = Set.insert name bounds <> usedTypeVariables replacement

          var' = genName var inUse

          ty' = replaceTypeVars' bounds var (TVariable var') ty
        in
          TForall var' (go (Set.insert var' bounds) ty') scope
      | otherwise = TForall var (go (Set.insert var bounds) ty) scope

    go _ ty = ty

-- Typeclass instances
instance showType :: Show Type where
  show (TUnknown num) = "t" <> show num
  show (TVariable name) = name
  show (TConstant name) = name
  show (Skolem name var id) = name
  show (TForall var ty _) = "forall " <> var <> ". " <> show ty
  show (TLambda from to) = prefix <> " -> " <> show to
    where
    prefix = if needsParenthesis from then "(" <> show from <> ")" else show from

instance substituableType :: Substituable Type Type where
  applySubstitution subst (TLambda from to) = TLambda (subst ?= from) (subst ?= to)
  applySubstitution (Substitution subst) ty@(TUnknown name) = fromMaybe ty $ Map.lookup name subst
  applySubstitution _ ty = ty

instance incompleteType :: Incomplete Type where
  unknown = TUnknown
  isUnknown (TUnknown name) = Just name
  isUnknown _ = Nothing
  unknowns (TUnknown v) = Set.singleton v
  unknowns (TLambda arg res) = unknowns arg <> unknowns res
  unknowns (TForall _ ty _) = unknowns ty
  unknowns _ = mempty
