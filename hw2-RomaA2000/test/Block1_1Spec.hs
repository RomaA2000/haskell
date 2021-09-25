module Block1_1Spec
    ( stringSumTestsTree
    ) where

import           Block1_1
import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Hspec

stringSumTestsTree :: IO TestTree
stringSumTestsTree = testSpec "String Sum" stringSumTests

stringSumTests :: Spec
stringSumTests = do
  describe "Summing" $ do
    it "summing correct string" $ do
      maybeSum "1 2 3 4 5 6 7" `shouldBe` Just 28
      maybeSum "1   3    2    10" `shouldBe` Just 16
      maybeSum "-52231 123 -987 8" `shouldBe` Just (-53087)
      maybeSum "52231 -21312 17867" `shouldBe` Just 48786
      maybeSum "5223 7867" `shouldBe` Just 13090
      maybeSum "12 \n\t 23 \n\t 45 \n  \t 67" `shouldBe` Just 147
      maybeSum "1 10 20 30" `shouldBe` Just 61
      maybeSum "1 " `shouldBe` Just 1

    it "summing incorrect string" $ do
      maybeSum "12 qw 12" `shouldBe` Nothing
      maybeSum "1 ad  3 da   2    10" `shouldBe` Nothing
      maybeSum "-52231 da  123 -987 8" `shouldBe` Nothing
      maybeSum "52231fff -21312 17867" `shouldBe` Nothing
      maybeSum "5223ads 7867" `shouldBe` Nothing
      maybeSum "12 23 45 67asd" `shouldBe` Nothing
      maybeSum "1 10 20 30 a" `shouldBe` Nothing
      maybeSum "1 f" `shouldBe` Nothing