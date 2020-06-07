module Main where

import Prelude
import Data.Either (Either(..), either)
import Data.Identity (Identity(..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Node.ReadLine (Interface, createConsoleInterface, noCompletion)
import Node.ReadLine.Aff (prompt, setPrompt)
import Node.ReadLine.Aff as RL
import Steiner.Control.Monad.Effect (print, printError)
import Steiner.Language.Parser (expression)
import Text.Parsing.Parser (runParserT)

replPrompt :: forall m. MonadEffect m => Interface -> m Unit
replPrompt = setPrompt "$ "

repl :: forall m. MonadAff m => Interface -> m Unit
repl interface = do
  replPrompt interface
  str <- prompt interface
  let
    (Identity tokens) = runParserT str expression
  case tokens of
    Left err -> printError err
    Right tokens -> do
      print tokens
      repl interface

main :: Effect Unit
main = do
  interface <- createConsoleInterface noCompletion
  runAff_
    ( either
        (\err -> printError err *> RL.close interface)
        (const $ RL.close interface)
    )
    (repl interface)
