module Main where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Identity (Identity(..))
import Data.Map as Map
import Data.String (joinWith)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (clear)
import Node.Process (exit)
import Node.ReadLine (Interface, createConsoleInterface, noCompletion)
import Node.ReadLine.Aff (prompt, setPrompt)
import Node.ReadLine.Aff as RL
import Steiner.Control.Monad.Effect (print, printError, printString)
import Steiner.Control.Monad.Infer (InferOutput(..), runInferT)
import Steiner.Control.Monad.Unify (Substitution(..), UnifyState(..), runUnifyT, unify)
import Steiner.Language.Parser (replCommand)
import Steiner.Language.Repl (Command(..))
import Steiner.Language.TypeCheck.Infer (infer)
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
        Exec ast -> case runExcept $ runInferT $ infer ast of
          Right (Tuple (Tuple ty (InferOutput { constraints })) st) -> do
            printString "Executing code isn't a thing yet so here is some random data about the input:"
            printString $ "The expression has inferred type: " <> show ty
            unless (Array.null constraints) do
              printString "Constraints:"
              printString $ joinWith "\n"
                $ ( uncurry \left right ->
                      let
                        unificationResult = map (const unit) $ runExcept $ runUnifyT (unify left right) st
                      in
                        case unificationResult of
                          Right _ -> show left <> " ~ " <> show right
                          Left err -> show err
                  )
                <$> constraints
          Left err -> do
            print err
        TypeOf ast -> case runExcept $ runInferT $ infer ast of
          Right (Tuple (Tuple ty (InferOutput { constraints })) st) -> do
            print ty
          Left err -> do
            print err
        Unify type' type'' -> case runExcept $ runUnifyT (unify type' type'') $ UnifyState { nextVar: 0 } of
          Left err -> print err
          Right (Tuple (Substitution subst) _) -> printString $ joinWith "\n" $ (uncurry \key ty -> "t" <> show key <> " = " <> show ty) <$> Map.toUnfoldable subst
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
