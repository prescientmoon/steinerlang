module Steiner.Language.Error where

import Prelude
import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Data.List (List(..), (:))
import Data.Tuple (Tuple)
import Data.Variant (SProxy(..), Variant, inj, match)
import Steienr.Data.String (indent)
import Steiner.Language.Ast (Expression(..))
import Steiner.Language.Type (Type)

-- |
-- Possible places an error can occur at
--
data ErrorSource
  = TypeError Type
  | ValueError Expression

instance showErrorSource :: Show ErrorSource where
  show (TypeError t) = "type: " <> show t
  show (ValueError ast) = case ast of
    (Let name value _) -> case value of
      Lambda _ _ -> "function '" <> name <> "'"
      _ -> name
    Variable name -> "variable '" <> name <> "'"
    Lambda from _ -> "annonymous function '" <> from <> " -> ...'"
    _ -> ""

-- |
-- Kind of errors which can occur during unification
--
type UnificationErrorKinds r
  = ( cannotUnify :: Tuple Type Type
    , recusriveType ::
      { ty :: Type
      , varName :: String
      }
    | r
    )

-- |
-- Kind of errors which can occur during type inference
--
type InferenceErrorKinds r
  = ( notInScope :: String
    | r
    )

-- |
-- Pretty print an inference error
--
showInferenceError :: Variant (InferenceErrorKinds ()) -> String
showInferenceError =
  (prefix <> _)
    <<< match
        { notInScope: \name -> "Variable " <> name <> " is not in scope"
        }
  where
  prefix = "InferenceError: "

-- |
-- Helper to create a notInScope error
--
notInScope :: String -> SteinerError (InferenceErrorKinds ())
notInScope name =
  SteinerError
    { error: inj (SProxy :: SProxy "notInScope") name
    , showErr: showInferenceError
    , source: Nil
    }

-- |
-- General type for errors
--
newtype SteinerError e
  = SteinerError
  { error :: Variant e
  , source :: List ErrorSource
  , showErr :: Variant e -> String
  }

-- |
-- Errors which occur during unification
--
type UnificationErrors
  = UnificationErrorKinds ()

-- |
-- Errors which occur during type infernece
--
type InferenceErrors
  = SteinerError (InferenceErrorKinds ())

-- |
-- Print a stack of errors
--
showStack :: List ErrorSource -> String
showStack Nil = ""

showStack (source : sources) = if location == "" then next else indent 1 $ "at " <> location <> "\n" <> next
  where
  location = show source

  next = showStack sources

instance showSteinerError :: Show (SteinerError e) where
  show (SteinerError { error, source, showErr }) = showErr error <> "\n" <> showStack source

-- |
-- Propagate all errors adding a new entry to the stack
--
propagateErrors :: forall m e a. MonadError (SteinerError e) m => ErrorSource -> m a -> m a
propagateErrors source =
  flip catchError \(SteinerError err) ->
    throwError $ SteinerError err { source = source : err.source }
