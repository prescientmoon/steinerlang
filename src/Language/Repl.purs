module Steiner.Language.Repl where

import Prelude
import Steiner.Language.Ast (Expression)
import Steiner.Language.Type (Type)

-- |
-- Different commands we can interpret in the repl
--
data Command
  = TypeOf Expression
  | Unify Type Type
  -- This is what happens by default when the user doesn't specify what to do
  | Exec Expression

-- |
-- Shortcuts for those commands used for parsing
--
replCommands :: Array String
replCommands = (":" <> _) <$> [ "t", "u" ]
