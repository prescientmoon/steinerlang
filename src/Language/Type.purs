module Steiner.Language.Type where

import Prelude
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Steiner.Control.Monad.Unify (class Incomplete, class Substituable, Substitution(..), Unknown, (?=))

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

-- |
-- Check if a type is a function 
--
isFunction :: Type -> Boolean
isFunction (TLambda _ _) = true

isFunction _ = false

-- Types for literals
typeInt :: Type
typeInt = TConstant "Int"

typeFloat :: Type
typeFloat = TConstant "Float"

typeString :: Type
typeString = TConstant "String"

typeBoolean :: Type
typeBoolean = TConstant "Boolean"

instance showType :: Show Type where
  show (TUnknown num) = "t" <> show num
  show (TConstant name) = name
  show (TLambda from to) = prefix <> " -> " <> show to
    where
    prefix = if isFunction from then "(" <> show from <> ")" else show from

instance substituableType :: Substituable Type Type where
  applySubstitution subst (TLambda from to) = TLambda (subst ?= from) (subst ?= to)
  applySubstitution (Substitution subst) ty@(TUnknown name) = fromMaybe ty $ Map.lookup name subst
  applySubstitution _ ty = ty

instance incompleteType :: Incomplete Type where
  unknown = TUnknown
  isUnknown (TUnknown name) = Just name
  isUnknown _ = Nothing
  unknowns = mempty
