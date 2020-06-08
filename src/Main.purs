module Main where

import Prelude
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
import Steiner.Language.Parser (expression)
import Steiner.Language.TypeCheck.Infer (infer)
import Text.Parsing.Parser (runParserT)

replPrompt :: forall m. MonadEffect m => Interface -> m Unit
replPrompt = setPrompt "$ "

repl :: forall m. MonadAff m => Interface -> m Unit
repl interface = do
  replPrompt interface
  str <- prompt interface
  let
    (Identity ast) = runParserT str expression
  case ast of
    Left err -> printError err
    Right ast -> do
      print ast
      let
        (Identity (Tuple (Tuple ty (InferOutput { constraints })) _)) = runInferT $ infer ast
      printString $ "The expression has inferred type: " <> show ty
      printString "Constraints:"
      printString $ joinWith "\n" $ (uncurry \left right -> show left <> " ~ " <> show right) <$> constraints
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
