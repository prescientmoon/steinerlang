module Language.TypeCheck.Check.Spec (spec) where

import Prelude
import Control.Monad.Error.Class (class MonadError)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Effect.Exception (Error)
import Steienr.Language.TypeCheck.TypeCheck (check, introduceSkolemScopes)
import Steiner.Language.Parser (expression, parseType)
import Test.Spec (Spec, describe, it)
import Test.Utils (runExceptCheck, shouldFail, shouldNotFail)
import Text.Parsing.Parser (runParser)

-- |
-- Perform type checking on 2 strings
--
stringChecking :: String -> String -> Either String Unit
stringChecking left right = do
  expression <- lmap show $ runParser left expression
  ty <- lmap show $ runParser right parseType
  lmap show $ runExceptCheck
    $ do
        ty'' <- introduceSkolemScopes ty
        void $ check expression ty''

-- |
-- Assert an expression having a certain type
--
shouldHaveType :: forall m. MonadError Error m => String -> String -> m Unit
shouldHaveType expr ty = shouldNotFail $ stringChecking expr ty

-- |
-- Assert an expression not having a certain type
--
shouldNotHaveType :: forall m. MonadError Error m => String -> String -> m Unit
shouldNotHaveType expr ty = shouldFail $ stringChecking expr ty

spec :: Spec Unit
spec =
  describe "Type checking" do
    describe "Literals" do
      describe "Ints" do
        it "should assert int literals being Ints" do
          "0" `shouldHaveType` "Int"
        it "should not accept int literals as strings" do
          "0" `shouldNotHaveType` "String"
      describe "Strings" do
        it "should assert string literals being Strings" do
          "\"Somehing\"" `shouldHaveType` "String"
        it "should not accept string literals as ints" do
          "\"Some string\"" `shouldNotHaveType` "Int"
      describe "Numbers" do
        it "should not consider ints floats" do
          "0" `shouldNotHaveType` "Float"
        it "should not consider floats ints" do
          "0.0" `shouldNotHaveType` "Int"
    describe "Variables" do
      it "should throw errors when referencing a variable which is not in scope" do
        "a" `shouldNotHaveType` "Int"
      it "should allow referencing variables" do
        "\\a -> if a then 0.0 else 1.0" `shouldHaveType` "Boolean -> Float"
      it "should allow using variables in less polymorphic contexts" do
        "\\a -> if a then a else 0" `shouldHaveType` "(forall a. a) -> Int"
      it "should not allow using varianles in more polymorphic contexts" do
        "\\a -> (a :: forall a. a) :: Int" `shouldNotHaveType` "(forall a. a -> a) -> Int"
      it "should respect variable shadowing" do
        "\\id -> \\a -> if a then \\a -> a else id" `shouldHaveType` "(forall a. a -> a) -> Boolean -> forall a. a -> a"
    describe "If expressions" do
      it "should accept the condition to be a boolean" do
        "\\a -> if a then 0 else 1" `shouldHaveType` "Boolean -> Int"
      it "should not accept other types as conditions" do
        "if 0 then 0 else 1" `shouldNotHaveType` "Int"
      it "should make sure both branches have the required type" do
        "\\a -> if a then 0 else 1.0" `shouldNotHaveType` "Boolean -> Int"
        "\\a -> if a then 0 else 1.0" `shouldNotHaveType` "Boolean -> Float"
    describe "Type annotations" do
      it "should accept annotations which are less polymorphic" do
        let
          id = "\\a -> a"
        id `shouldHaveType` "forall a. a -> a"
        id `shouldHaveType` "Int -> Int"
        id `shouldHaveType` "forall a. (Int -> a) -> (Int -> a)"
      it "should not accept annotations which are more polymorphic" do
        "0 :: forall a. a" `shouldNotHaveType` "forall a. a"
        "(\\a -> a) :: Int -> Int" `shouldNotHaveType` "forall a. a -> a"
    describe "Lambdas" do
      it "should add the argument in scope" do
        "\\a -> a" `shouldHaveType` "Float -> Float"
      it "should accept polymorhpic arguments" do
        "\\a -> a" `shouldHaveType` "(forall a. a) -> Int"
        "\\a -> a" `shouldHaveType` "forall b. (forall a. a -> a) -> b -> b"
        "\\a -> a :: forall a. a -> a" `shouldHaveType` "(forall a. a) -> Int -> Int"
      it "should not allow using the argument as the wrong type" do
        "\\a -> if a then a else a" `shouldNotHaveType` "Int -> Int"
        "\\a -> a :: forall a. a" `shouldNotHaveType` "(forall a. a -> a) -> forall a. a"
