module Main where

import Prelude
import Data.Either (Either(..), either)
import Data.Identity (Identity(..))
import Data.Lens (view)
import Data.String (joinWith)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (clear)
import Node.Process (exit)
import Node.ReadLine (Interface, createConsoleInterface, noCompletion)
import Node.ReadLine.Aff (prompt, setPrompt)
import Node.ReadLine.Aff as RL
import Steienr.Data.String (indent)
import Steienr.Language.TypeCheck.TypeCheck (Typed(..), check, infer, introduceSkolemScopes, subsumes, unify)
import Steiner.Control.Monad.Check (runWithUnifyT)
import Steiner.Control.Monad.Effect (print, printError, printString)
import Steiner.Control.Monad.Unify (_currentSubstitution, runUnifyT, (?=))
import Steiner.Language.Parser (replCommand)
import Steiner.Language.Repl (Command(..))
import Text.Parsing.Parser (runParserT)

replPrompt :: forall m. MonadEffect m => Interface -> m Unit
replPrompt = setPrompt "$ "

-- |
-- Prase and execute a string.
--
execCommand :: forall m. MonadEffect m => Interface -> String -> m Unit
execCommand interface str =
  let
    (Identity parsingResult) = runParserT str replCommand
  in
    case parsingResult of
      Left err -> printError err
      Right command -> do
        let
          (result :: Either _ _) = case command of
            Exec ast -> pure $ printString "Executing code isn't a thing yet:)"
            TypeOf ast -> do
              Tuple (Typed _ newExpr type') state <-
                runUnifyT do
                  infer ast
              let
                sub = view _currentSubstitution state
              pure $ printString
                $ joinWith "\n"
                    [ "Type inference finished succesfully!"
                    , "The expression has type"
                    , indent 4 $ show $ sub ?= type'
                    , "and the new expression looks like"
                    , indent 4 $ show newExpr
                    ]
            ViewAst ast -> pure $ print ast
            Unify type' type'' -> do
              st <-
                runUnifyT do
                  type''' <- introduceSkolemScopes type'
                  type'''' <- introduceSkolemScopes type''
                  unify type''' type''''
              let
                sub = view _currentSubstitution $ snd st
              pure do
                print $ sub ?= type'
                print $ sub ?= type''
            Subsumes type' type'' -> do
              st <-
                runUnifyT do
                  type''' <- introduceSkolemScopes type'
                  type'''' <- introduceSkolemScopes type''
                  subsumes type''' type''''
              pure $ printString "Subsumption went fine!"
            Check ast type' -> do
              void
                $ runWithUnifyT
                $ introduceSkolemScopes type'
                >>= check ast
              pure
                $ printString
                    "Type checking went fine!"
            Quit ->
              pure do
                RL.close interface
                liftEffect $ exit 0
            NoCommand -> pure $ pure unit
            Clear -> pure clear
            InvalidCommand name ->
              pure do
                printString
                  $ joinWith "\n"
                      [ "Unknown command \"" <> name <> "\""
                      , "Run :help to get a list of all available commands."
                      , "(Hint: I didn't implement the help command yet)"
                      ]
        case result of
          Left err -> printError err
          Right effect -> effect

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
  execCommand interface str
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
