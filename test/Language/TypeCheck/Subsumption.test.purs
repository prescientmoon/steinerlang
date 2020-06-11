module Language.TypeCheck.Subsumption.Spec where

import Prelude
import Control.Monad.Error.Class (class MonadError)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Effect.Exception (Error)
import Steienr.Language.TypeCheck.TypeCheck (introduceSkolemScopes, subsumes)
import Steiner.Language.Parser (parseType)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)
import Test.Utils (runExceptUnify, shouldNotFail)
import Text.Parsing.Parser (runParser)

-- |
-- Perform subsumption on 2 strings
--
stringSubsumption :: String -> String -> Either String Unit
stringSubsumption left right = do
  ty <- lmap show $ runParser left parseType
  ty' <- lmap show $ runParser right parseType
  lmap show $ runExceptUnify
    $ do
        ty'' <- introduceSkolemScopes ty
        ty''' <- introduceSkolemScopes ty'
        subsumes ty'' ty'''

-- |
-- Assert a type subsuming another
--
shouldSubsume :: forall m. MonadError Error m => String -> String -> m Unit
shouldSubsume left right = shouldNotFail $ stringSubsumption left right

-- |
-- Assert a type not subsuming another
--
shouldNotSubsume :: forall m. MonadError Error m => String -> String -> m Unit
shouldNotSubsume left right = case stringSubsumption left right of
  Left _ -> pure unit
  Right _ -> fail $ "Type " <> left <> " should not subsume type " <> right

-- |
-- Both arguments must subsume no matter the order.
--
shouldBeEquivalentWith :: forall m. MonadError Error m => String -> String -> m Unit
shouldBeEquivalentWith left right = do
  left `shouldSubsume` right
  right `shouldSubsume` left

-- |
-- The first type should subsume the second but not the other way around.
--
shouldStrictlySubsume :: forall m. MonadError Error m => String -> String -> m Unit
shouldStrictlySubsume left right = do
  left `shouldSubsume` right
  right `shouldNotSubsume` left

spec :: Spec Unit
spec =
  describe "The subsumption relation" do
    describe "Tau types" do
      it "should pass for equal opaque types" do
        "Int" `shouldSubsume` "Int"
        "Bool" `shouldSubsume` "Bool"
        "String" `shouldSubsume` "String"
      it "should fail for different opaque types" do
        "Int" `shouldNotSubsume` "String"
        "Bool" `shouldNotSubsume` "Int"
    describe "Rank n types" do
      it "should allow anything to be less polymorphic than forall a. a" do
        "Int" `shouldStrictlySubsume` "forall a. a"
        "Bool" `shouldStrictlySubsume` "forall a. a"
        "forall a. a -> a" `shouldStrictlySubsume` "forall a. a"
        "(forall a. a) -> Int" `shouldStrictlySubsume` "forall a. a"
        "forall b. (forall a. a -> forall a. a) -> (b -> forall c.c)" `shouldStrictlySubsume` "forall a. a"
        "forall a. a" `shouldSubsume` "forall a. a"
      it "should not care about the number of quantifiers" do
        "forall a. (a -> Int) -> (a -> Int)" `shouldStrictlySubsume` "forall a. a -> a" -- both have the same number of quantifiers
        "forall a. forall b. (a -> b) -> (a -> b)" `shouldStrictlySubsume` "forall a. a -> a" -- first has more quantifiers
        "forall b. (b -> b) -> (b -> b)" `shouldStrictlySubsume` "forall b. forall c. (b -> c) -> (c -> b)" -- second has more quantifiers
      it "should not care about moving the forall outwards from positive positions" do
        "forall a. Int -> a" `shouldBeEquivalentWith` "Int -> forall a. a"
      it "should not allow moving the forall outwards out of negative positions" do
        "forall a. a -> Int" `shouldNotSubsume` "(forall a. a) -> Int"
      it "should reverse the relation in negative positions" do
        "forall a. Int -> a" `shouldStrictlySubsume` "forall a. a"
        "(forall a. a) -> Bool" `shouldStrictlySubsume` "(forall a. Int -> a) -> Bool"
      it "should respect variable shadowing" do
        "forall a. (forall b. b) -> a" `shouldSubsume` "forall a. (forall a. a -> a) -> a"
