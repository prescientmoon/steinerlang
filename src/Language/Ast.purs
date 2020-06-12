module Steiner.Language.Ast
  ( Literal(..)
  , Expression(..)
  ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Steiner.Language.Type (Type)

data Literal
  = StringLit String
  | FloatLit Number
  | IntLit Int

derive instance genericLiteral :: Generic Literal _

derive instance eqLiteral :: Eq Literal

instance showLiteral :: Show Literal where
  show = genericShow

data Expression
  = If Expression Expression Expression
  | Let String Expression Expression
  | Lambda String Expression
  | Variable String
  | Literal Literal
  | Application Expression Expression
  | TypedExpression Boolean Expression Type

infixl 4 Application as ~~>

derive instance genericExpression :: Generic Expression _

derive instance eqExpression :: Eq Expression

instance showExpression :: Show Expression where
  show expr = genericShow expr
