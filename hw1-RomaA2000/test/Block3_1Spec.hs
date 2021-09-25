module Block3_1Spec (maybeEitherConcatTestsTree) where

import           Block3_1
import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Hspec

maybeEitherConcatTestsTree :: IO TestTree
maybeEitherConcatTestsTree = testSpec "Maybe and either concat" maybeEitherConcatTests

maybeEitherConcatTests :: Spec
maybeEitherConcatTests = do
  describe "maybeConcat" $
    it "concatenates maybe elements" $ do
      maybeConcat [] `shouldBe` ([] :: String)
      maybeConcat [Nothing, Just "str", Nothing] `shouldBe` "str"
      maybeConcat [Nothing, Just "s", Nothing, Nothing, Just "t", Just "r"] `shouldBe` "str"
      maybeConcat [Just "s", Nothing, Just "tr", Just "r"] `shouldBe` "strr"
      maybeConcat [Just "ss", Nothing, Just "tr", Just "r"] `shouldBe` "sstrr"
      maybeConcat [Just [1, 2], Nothing, Just [], Just [3, 4]] `shouldBe` [1, 2, 3, 4]

  describe "eitherConcat" $
    it "concatenates either elements" $ do
      eitherConcat [Left "1", Right [1, 2, 3], Left "23", Right [4, 5]] `shouldBe` ("123", [1, 2, 3, 4, 5])
      eitherConcat [Left "1", Left "23", Right [1]] `shouldBe` ("123", [1])
      eitherConcat [Right [1, 2, 3], Right [4, 5]] `shouldBe` ("", [1, 2, 3, 4, 5])
      eitherConcat [Left "", Right [1, 4, 3], Left "23", Left "01", Right [], Right [5]] `shouldBe` ("2301", [1, 4, 3, 5])
