module Main where

import Prelude
import Data.Either (Either(..), either)
import Data.Identity (Identity(..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (clear)
import Node.Process (exit)
import Node.ReadLine (Interface, createConsoleInterface, noCompletion)
import Node.ReadLine.Aff (prompt, setPrompt)
import Node.ReadLine.Aff as RL
import Steiner.Control.Monad.Effect (printError, printString)
import Steiner.Language.Parser (replCommand)
import Steiner.Language.Repl (Command(..))
import Text.Parsing.Parser (runParserT)

replPrompt :: forall m. MonadEffect m => Interface -> m Unit
replPrompt = setPrompt "$ "

-- |
-- This is a very basic repl I use in development.
-- It shows me the type of an expression, 
-- prints the error which might occur along the way,
-- and calls itself recursively at the end.
--
repl :: forall m. MonadAff m => Interface -> m Unit
repl interface = do
  replPrompt interface
  str <- prompt interface
  let
    (Identity parsingResult) = runParserT str replCommand
  case parsingResult of
    Left err -> printError err
    Right cmd -> do
      case cmd of
        Exec ast -> printString "Executing code isn't a thing yet so here is some random data about the input:"
        TypeOf ast -> printString "todo"
        Unify type' type'' -> printString "todo"
        Quit -> do
          RL.close interface
          liftEffect $ exit 0
        NoCommand -> pure unit
        Clear -> clear
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
