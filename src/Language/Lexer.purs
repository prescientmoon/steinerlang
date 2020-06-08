module Steiner.Language.Lexer (tokenParser) where

import Prelude
import Control.MonadPlus ((<|>))
import Data.Identity (Identity)
import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.String (class StringLike, oneOf)
import Text.Parsing.Parser.Token (GenLanguageDef(..), GenTokenParser, LanguageDef, alphaNum, letter, makeTokenParser)

opChars :: forall s m. StringLike s => Monad m => ParserT s m Char
opChars = oneOf [ ':', '!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~' ]

language :: LanguageDef
language =
  LanguageDef
    { commentStart: "{-"
    , commentEnd: "-}"
    , commentLine: "--"
    , nestedComments: true
    , opStart: opChars
    , opLetter: opChars
    , caseSensitive: true
    , reservedOpNames: [ "::", "->", "::", "\\" ]
    , reservedNames: [ "if", "then", "else", "let", "in" ]
    , identStart: letter
    , identLetter: alphaNum <|> oneOf [ '_', '\'' ]
    }

tokenParser :: GenTokenParser String Identity
tokenParser = makeTokenParser language
