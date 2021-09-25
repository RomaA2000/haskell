module Block1_2Spec (natTestsTree) where

import           Block1_2
import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Hspec

natTestsTree :: IO TestTree
natTestsTree = testSpec "Nat" natTests

natTests :: Spec
natTests = do
  describe "eq" $
      it "== for nuts" $ do
        (==) (natFromInt 1) (natFromInt 1) `shouldBe` True
        (==) (natFromInt 2) (natFromInt 2) `shouldBe` True
        (==) (natFromInt 1) (natFromInt 10) `shouldBe` False
        (==) (natFromInt 1) (natFromInt 0) `shouldBe` False
        (==) (natFromInt 2) (natFromInt 2) `shouldBe` True
        (==) (natFromInt 1) (natFromInt 7) `shouldBe` False
        (==) (natFromInt 7) (natFromInt 1) `shouldBe` False
        (==) (natFromInt 100) (natFromInt 0) `shouldBe` False

  describe "ord" $
      it "<=> for nuts" $ do
        (<=) (natFromInt 1) (natFromInt 1) `shouldBe` True
        (>=) (natFromInt 2) (natFromInt 2) `shouldBe` True
        (>) (natFromInt 1) (natFromInt 10) `shouldBe` False
        (>) (natFromInt 1) (natFromInt 0) `shouldBe` True
        (<) (natFromInt 2) (natFromInt 2) `shouldBe` False
        (<) (natFromInt 1) (natFromInt 7) `shouldBe` True
        (>=) (natFromInt 7) (natFromInt 1) `shouldBe` True
        (<=) (natFromInt 100) (natFromInt 0) `shouldBe` False


  describe "sum" $
    it "summing nuts" $ do
     (+) (natFromInt 1) (natFromInt 1) `shouldBe` (natFromInt 2)
     (+) (natFromInt 1) (natFromInt 0) `shouldBe` (natFromInt 1)
     (+) (natFromInt 0) (natFromInt 0) `shouldBe` (natFromInt 0)
     (+) (natFromInt 0) (natFromInt 0) `shouldBe` Z

  describe "mul" $
    it "multiplying nuts" $ do
      (*) (natFromInt 1) (natFromInt 1) `shouldBe` (natFromInt 1)
      (*) (natFromInt 2) (natFromInt 2) `shouldBe` (S $ S $ S $ S $ Z)
      (*) (natFromInt 10) (natFromInt 10) `shouldBe` (natFromInt 100)
      (*) (natFromInt 1) (natFromInt 0) `shouldBe` (natFromInt 0)
      (*) (natFromInt 2) (natFromInt 3) `shouldBe` (natFromInt 6)
      (*) (natFromInt 1) (natFromInt 7) `shouldBe` (natFromInt 7)
      (*) (natFromInt 7) (natFromInt 1) `shouldBe` (natFromInt 7)
      (*) (natFromInt 100) (natFromInt 0) `shouldBe` Z

  describe "is even" $
    it "even check for nuts" $ do
      isEven (natFromInt 1) `shouldBe` False
      isEven (natFromInt 0) `shouldBe` True
      isEven (natFromInt 3) `shouldBe` False
      isEven (natFromInt 4) `shouldBe` True

  describe "div" $
    it "dividing nuts" $ do
      divNat (natFromInt 1) (natFromInt 1) `shouldBe` (natFromInt 1)
      divNat (natFromInt 3) (natFromInt 1) `shouldBe` (natFromInt 3)
      divNat (natFromInt 3) (natFromInt 2) `shouldBe` (natFromInt 1)
      divNat (natFromInt 5) (natFromInt 6) `shouldBe` (natFromInt 0)
      divNat (natFromInt 0) (natFromInt 2) `shouldBe` (natFromInt 0)
      divNat (natFromInt 10) (natFromInt 3) `shouldBe` (natFromInt 3)

  describe "mod" $
    it "mod nuts" $ do
      modNat (natFromInt 1) (natFromInt 1) `shouldBe` (natFromInt 0)
      modNat (natFromInt 3) (natFromInt 1) `shouldBe` (natFromInt 0)
      modNat (natFromInt 3) (natFromInt 2) `shouldBe` (natFromInt 1)
      modNat (natFromInt 5) (natFromInt 6) `shouldBe` (natFromInt 5)
      modNat (natFromInt 0) (natFromInt 2) `shouldBe` (natFromInt 0)
      modNat (natFromInt 10) (natFromInt 3) `shouldBe` (natFromInt 1)
