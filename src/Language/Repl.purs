module Steiner.Language.Repl where

import Prelude
import Steiner.Language.Ast (Expression)
import Steiner.Language.Type (Type)

-- |
-- Different commands we can interpret in the repl
--
data Command
  -- |
  -- Command to get the type of an expression
  --
  = TypeOf Expression
  -- |
  -- Command to unify 2 types and see the resulting substitution
  --
  | Unify Type Type
  -- |
  -- Command to check if a type is at least as polymorphic as another
  --
  | Subsumes Type Type
  -- |
  -- Command to execute some code. If no command is specified this is used by default
  --
  | Exec Expression
  -- |
  -- This command just quits the repl
  --
  | Quit
  -- |
  -- If the user didn't input anything we can continue asking for input
  --
  | NoCommand
  -- |
  -- Command to clear the console. I might remove this later.
  --
  | Clear

-- |
-- Shortcuts for those commands used for parsing
--
replCommands :: Array String
replCommands = (":" <> _) <$> [ "t", "u", "q", "s", "clear" ]
