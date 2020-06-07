module Steiner.Language.Parser (expression) where

import Control.Lazy (fix)
import Control.MonadPlus ((<|>))
import Data.Either (either)
import Data.Identity (Identity)
import Prelude (bind, discard, pure, ($), (<$>))
import Steiner.Language.Ast (Expression(..), Literal(..))
import Steiner.Language.Lexer (tokenParser)
import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.String (eof)

-- Parser for individual steiner expressions
-- This references itself so we use it within a fixpoint operator
expression' :: ParserT String Identity Expression -> ParserT String Identity Expression
expression' expr = wrapped <|> ifExpr <|> letExpr <|> literal <|> variable
  where
  { parens, identifier, reserved, reservedOp, stringLiteral, naturalOrFloat } = tokenParser

  wrapped = parens expr

  variable = Variable <$> identifier

  ifExpr = do
    reserved "if"
    condition <- expr
    reserved "then"
    then' <- expr
    reserved "else"
    else' <- expr
    pure $ If condition then' else'

  letExpr = do
    reserved "let"
    name <- identifier
    reservedOp "="
    value <- expr
    reserved "in"
    body <- expr
    pure $ Let name value body

  stringLiteral' = StringLit <$> stringLiteral

  numberLiteral = either IntLit FloatLit <$> naturalOrFloat

  literal = Literal <$> (stringLiteral' <|> numberLiteral)

-- The parser for the steiner syntax
expression :: ParserT String Identity Expression
expression = do
  ast <- fix expression'
  eof
  pure ast
