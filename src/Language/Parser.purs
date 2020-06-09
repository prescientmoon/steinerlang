module Steiner.Language.Parser (expression, replCommand) where

import Prelude
import Control.Lazy (fix)
import Control.MonadPlus ((<|>))
import Data.Either (either)
import Data.Identity (Identity)
import Steiner.Language.Ast (Expression(..), Literal(..))
import Steiner.Language.Lexer (tokenParser)
import Steiner.Language.Repl (Command(..))
import Text.Parsing.Parser (ParserT, Parser)
import Text.Parsing.Parser.String (eof)

-- Parser for individual steiner expressions
-- This references itself so we use it within a fixpoint operator
expression' :: ParserT String Identity Expression -> ParserT String Identity Expression
expression' expr = wrapped <|> ifExpr <|> letExpr <|> lambdaExpr <|> literal <|> variable
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

  lambdaExpr = do
    reservedOp "\\"
    arg <- identifier
    reservedOp "->"
    body <- expr
    pure $ Lambda arg body

  stringLiteral' = StringLit <$> stringLiteral

  numberLiteral = either IntLit FloatLit <$> naturalOrFloat

  literal = Literal <$> (stringLiteral' <|> numberLiteral)

-- The parser for the steiner syntax
expression :: ParserT String Identity Expression
expression = fix expression'

-- |
-- Parses a repl command
--
replCommand :: Parser String Command
replCommand = (typeOf <|> clear <|> quit <|> run <|> noCommand) <* eof
  where
  { reserved } = tokenParser

  typeOf = reserved ":t" *> (TypeOf <$> expression)

  clear = Clear <$ reserved ":clear"

  quit = Quit <$ reserved ":q"

  run = Exec <$> expression

  noCommand = NoCommand <$ eof
