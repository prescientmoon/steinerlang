module Steiner.Language.TypeCheck.Infer where

import Prelude
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Steiner.Control.Monad.Infer (InferT, lookupVar, withScope, (~~))
import Steiner.Control.Monad.Unify (fresh)
import Steiner.Language.Ast (Expression(..), Literal(..))
import Steiner.Language.Type (Type(..), typeBoolean, typeFloat, typeInt, typeString)

-- |
inferLiteral :: Literal -> Type
inferLiteral (IntLit _) = typeInt

inferLiteral (FloatLit _) = typeFloat

inferLiteral (StringLit _) = typeString

-- |
-- Constrain an expression to a certain type
--
constrainTo :: Expression -> Type -> InferT Identity Unit
constrainTo expr ty = do
  ty' <- infer expr
  ty' ~~ ty

-- |
-- Infer the type of an expression generating constrains along the way
--
infer :: Expression -> InferT Identity Type
infer (Literal literal) = pure $ inferLiteral literal

infer (If condition then' else') = do
  r <- fresh
  condition `constrainTo` typeBoolean
  then' `constrainTo` r
  else' `constrainTo` r
  pure r

infer (Lambda argName body) = do
  arg <- fresh
  ret <- withScope argName arg $ infer body
  pure $ TLambda arg ret

infer (Variable name) = do
  ty <- lookupVar name
  case ty of
    Just ty' -> pure ty'
    Nothing -> fresh

infer (Let name value body) = do
  typeValue <- infer value
  withScope name typeValue $ infer body
