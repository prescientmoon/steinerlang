module Main where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Identity (Identity(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Node.ReadLine (Interface, createConsoleInterface, noCompletion)
import Node.ReadLine.Aff (prompt, setPrompt)
import Node.ReadLine.Aff as RL
import Steiner.Control.Monad.Effect (print, printError, printString)
import Steiner.Control.Monad.Infer (InferOutput(..), runInferT)
import Steiner.Control.Monad.Unify (runUnifyT, unify)
import Steiner.Language.Parser (expression)
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
    (Identity ast) = runParserT str expression
  case ast of
    Left err -> printError err
    Right ast -> do
      case runExcept $ runInferT $ infer ast of
        Right (Tuple (Tuple ty (InferOutput { constraints })) st) -> do
          printString $ "The expression has inferred type: " <> show ty
          unless (Array.null constraints) do
            printString "Constraints:"
            printString $ joinWith "\n"
              $ ( uncurry \left right ->
                    let
                      unificationResult = map (const unit) $ runExcept $ runUnifyT (unify left right) st
                    in
                      show left <> " ~ " <> show right <> " => "
                        <> (show unificationResult)
                )
              <$> constraints
        Left err -> do
          print err
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
