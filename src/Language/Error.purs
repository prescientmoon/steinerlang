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
  show (TypeError t) = "TypeError (" <> show t <> ")"
  show (ValueError ast) = "ValueError (" <> show ast <> ")"

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
-- Error for when a variable which doesn't exist in the current scope
-- is accessed by the progrramer
--
newtype NotInScope
  = NotInScope String

instance showNotInScope :: Show NotInScope where
  show (NotInScope name) = "Variable " <> name <> " is not in scope."

-- |
-- Kind of errors which can occur during type inference
--
type InferenceErrorKinds r
  = ( notInScope :: NotInScope
    | r
    )

-- |
-- Helper to create a notInScope error
--
notInScope :: Maybe ErrorSource -> String -> SteinerError (InferenceErrorKinds ())
notInScope source name =
  SteinerError
    { error: inj (SProxy :: SProxy "notInScope") $ NotInScope name
    , showErr: match { notInScope: show :: NotInScope -> String }
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
  show (SteinerError { error, source, showErr }) = prefix <> showErr error
    where
    prefix = case source of
      Just actualSource -> show actualSource <> ": "
      Nothing -> ""
