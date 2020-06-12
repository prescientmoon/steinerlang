module Steiner.Language.Ast
  ( Literal(..)
  , Expression(..)
  , everywhereOnExpression
  , everywhereOnExpressionM
  ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Steiner.Language.Type (Type)

data Literal
  = StringLit String
  | FloatLit Number
  | IntLit Int

derive instance genericLiteral :: Generic Literal _

derive instance eqLiteral :: Eq Literal

instance showLiteral :: Show Literal where
  show = genericShow

-- |
-- The type for the Steiner Ast
--
data Expression
  = If Expression Expression Expression
  | Let String Expression Expression
  | Lambda String Expression
  | Variable String
  | Literal Literal
  | Application Expression Expression
  | TypedExpression Boolean Expression Type

-- |
-- Run a computation in a monad on every level of an expression
--
everywhereOnExpressionM :: forall m. Monad m => (Expression -> m Expression) -> Expression -> m Expression
everywhereOnExpressionM f = go'
  where
  go continue (If condition then' else') = do
    condition' <- continue condition
    then'' <- continue then'
    else'' <- continue else'
    pure $ If condition' then'' else''

  go continue (Let name value body) = do
    value' <- continue value
    body' <- continue body
    pure $ Let name value' body'

  go continue (Lambda arg body) = do
    body' <- continue body
    pure $ Lambda arg body'

  go continue (Application function argument) = do
    function' <- continue function
    argument' <- continue argument
    pure $ Application function' argument'

  go continue (TypedExpression check expression ty) = do
    expression' <- continue expression
    pure $ TypedExpression check expression' ty

  go _ other = pure other

  go' expression = do
    expression' <- f expression
    go go' expression'

-- |
-- Map each level of an expression
--
everywhereOnExpression :: (Expression -> Expression) -> Expression -> Expression
everywhereOnExpression f = unwrap <<< everywhereOnExpressionM (Identity <<< f)

infixl 4 Application as ~~>

derive instance genericExpression :: Generic Expression _

derive instance eqExpression :: Eq Expression

instance showExpression :: Show Expression where
  show expr = genericShow expr
