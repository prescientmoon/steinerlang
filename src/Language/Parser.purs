module Steiner.Language.Parser (expression, replCommand, parseType) where

import Prelude
import Control.Lazy (fix)
import Control.MonadPlus ((<|>))
import Data.Char.Unicode (isUpper)
import Data.Either (either)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..), maybe)
import Data.String.Yarn ((!!))
import Steiner.Language.Ast (Expression(..), Literal(..))
import Steiner.Language.Lexer (tokenParser)
import Steiner.Language.Repl (Command(..))
import Steiner.Language.Type (Type(..))
import Text.Parsing.Parser (ParserT, Parser)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.String (eof)

-- |
-- Parser for individual steiner expressions
-- This references itself so we use it within a fixpoint operator
--
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

-- |
-- The parser for the steiner syntax
--
expression :: ParserT String Identity Expression
expression = fix expression'

-- |
-- Parser for types
--
parseType' :: ParserT String Identity Type -> ParserT String Identity Type
parseType' type' = (try lambda) <|> nonLambda
  where
  { parens, identifier, reserved, reservedOp } = tokenParser

  typeVar = do
    name <- identifier
    pure
      if maybe false isUpper $ name !! 0 then
        TConstant name
      else
        TVariable name

  nonLambda = parens type' <|> parseForall <|> typeVar

  parseForall = do
    reserved "forall"
    var <- identifier
    reservedOp "."
    ty <- type'
    pure $ TForall var ty Nothing

  lambda = do
    from <- nonLambda
    reservedOp "->"
    to <- type'
    pure $ TLambda from to

-- |
-- THe parser for the steiner syntax for types 
--
parseType :: ParserT String Identity Type
parseType = fix parseType'

-- |
-- Parses a repl command
--
replCommand :: Parser String Command
replCommand = (typeOf <|> clear <|> quit <|> run <|> check <|> unify <|> subsumes <|> noCommand <|> invalidCommand) <* eof
  where
  { reserved, identifier, reservedOp } = tokenParser

  run = Exec <$> expression

  typeOf = reserved ":t" *> (TypeOf <$> expression)

  clear = Clear <$ reserved ":clear"

  quit = Quit <$ reserved ":q"

  unify = do
    reserved ":u"
    ty <- parseType
    ty' <- parseType
    pure $ Unify ty ty'

  subsumes = do
    reserved ":s"
    ty <- parseType
    ty' <- parseType
    pure $ Subsumes ty ty'

  check = do
    reserved ":check"
    ast <- expression
    ty <- parseType
    pure $ Check ast ty

  noCommand = NoCommand <$ eof

  invalidCommand = InvalidCommand <$> (reservedOp ":" *> identifier)
