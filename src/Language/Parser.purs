module Steiner.Language.Parser (expression) where

import Control.Lazy (fix)
import Control.MonadPlus ((<|>))
import Data.Identity (Identity)
import Prelude (bind, discard, pure, ($), (<$>))
import Steiner.Language.Ast (Expression(..))
import Steiner.Language.Lexer (tokenParser)
import Text.Parsing.Parser (ParserT)

expression' :: ParserT String Identity Expression -> ParserT String Identity Expression
expression' expr = wrapped <|> ifExpr <|> variable
  where
  { parens, identifier, reserved } = tokenParser

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

expression :: ParserT String Identity Expression
expression = fix expression'
