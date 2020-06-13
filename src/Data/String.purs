module Steienr.Data.String where

import Prelude
import Data.String (joinWith)
import Data.String.Utils (lines, unsafeRepeat)

-- |
-- Indent a string by a number of spaces
--
indent :: Int -> String -> String
indent spaces = joinWith "\n" <<< map (space <> _) <<< lines
  where
  space = unsafeRepeat spaces " "

-- |
-- Surround a string with another
--
surroundWith :: String -> String -> String
surroundWith bounds string = bounds <> string <> bounds

-- |
-- Add quotes around a string
--
quoted :: String -> String
quoted = surroundWith "\""

-- |
-- Put spaces around a string
--
spaced :: String -> String
spaced = surroundWith " "
