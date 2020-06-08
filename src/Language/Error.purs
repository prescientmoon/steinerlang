module Steiner.Language.Error where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Variant (SProxy(..), Variant, inj, match)
import Steiner.Language.Ast (Expression)
import Steiner.Language.Type (Type)

-- |
-- Possible places an error can occur at
--
data ErrorSource
  = TypeError Type
  | ValueError Expression

instance showErrorSource :: Show ErrorSource where
  show (TypeError t) = "type: " <> show t
  show (ValueError ast) = "value: " <> show ast

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
notInScope :: Maybe ErrorSource -> String -> SteinerError (InferenceErrorKinds ())
notInScope source name =
  SteinerError
    { error: inj (SProxy :: SProxy "notInScope") name
    , showErr: showInferenceError
    , source
    }

-- |
-- General type for errors
--
newtype SteinerError e
  = SteinerError
  { error :: Variant e
  , source :: Maybe ErrorSource
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

instance showSteinerError :: Show (SteinerError e) where
  show (SteinerError { error, source, showErr }) =
    showErr error
      <> case source of
          Just actualSource -> "\n    at " <> show actualSource
          Nothing -> ""
