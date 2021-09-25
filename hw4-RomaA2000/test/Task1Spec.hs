module Task1Spec
    (geometryTestsTree
    ) where

import qualified Task1Perf as P
import qualified Task1Norm as N
import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Hspec

geometryTestsTree :: IO TestTree
geometryTestsTree = testSpec "Geometry" geometryTests

geometryTests :: Spec
geometryTests = do
  describe "simple operations" $ do
    it "plus" $ do
      N.plus (N.Point 1 2) (N.Point 1 2) `shouldBe` N.Point 2 4
      P.plus (P.Point 1 2) (P.Point 1 2) `shouldBe` P.Point 2 4
      N.plus (N.Point 3 1) (N.Point 0 1) `shouldBe` N.Point 3 2
      P.plus (P.Point 3 1) (P.Point 0 1) `shouldBe` P.Point 3 2
      N.plus (N.Point 5 0) (N.Point 2 3) `shouldBe` N.Point 7 3
      P.plus (P.Point 5 0) (P.Point 2 3) `shouldBe` P.Point 7 3
    it "minus" $ do
      N.minus (N.Point 1 2) (N.Point 1 2) `shouldBe` N.Point 0 0
      P.minus (P.Point 1 2) (P.Point 1 2) `shouldBe` P.Point 0 0
      N.minus (N.Point 3 1) (N.Point 0 1) `shouldBe` N.Point 3 0
      P.minus (P.Point 3 1) (P.Point 0 1) `shouldBe` P.Point 3 0
      N.minus (N.Point 5 0) (N.Point 2 3) `shouldBe` N.Point 3 (-3)
      P.minus (P.Point 5 0) (P.Point 2 3) `shouldBe` P.Point 3 (-3)
  describe "product operations" $ do
    it "scalar" $ do
      N.scalarProduct (N.Point 1 2) (N.Point 1 2) `shouldBe` 5
      P.scalarProduct (P.Point 1 2) (P.Point 1 2) `shouldBe` 5
      N.scalarProduct (N.Point 3 1) (N.Point 4 0) `shouldBe` 12
      P.scalarProduct (P.Point 3 1) (P.Point 4 0) `shouldBe` 12
      N.scalarProduct (N.Point 0 1) (N.Point 1 1) `shouldBe` 1
      P.scalarProduct (P.Point 0 1) (P.Point 1 1) `shouldBe` 1
    it "cross" $ do
      N.crossProduct (N.Point 1 2) (N.Point 1 2) `shouldBe` 0
      P.crossProduct (P.Point 1 2) (P.Point 1 2) `shouldBe` 0
      N.crossProduct (N.Point 3 1) (N.Point 4 0) `shouldBe` -4
      P.crossProduct (P.Point 3 1) (P.Point 4 0) `shouldBe` -4
      N.crossProduct (N.Point 0 1) (N.Point 1 1) `shouldBe` -1
      P.crossProduct (P.Point 0 1) (P.Point 1 1) `shouldBe` -1
  describe "perimeter and area operations" $ do
    it "perimeter" $ do
      N.perimeter [(N.Point 1 1), (N.Point 1 0), (N.Point 0 0), (N.Point 0 1)] `shouldBe` 4.0
      P.perimeter [(P.Point 1 1), (P.Point 1 0), (P.Point 0 0), (P.Point 0 1)] `shouldBe` 4.0
      N.perimeter [(N.Point 2 2), (N.Point 2 0), (N.Point 0 0), (N.Point 0 2)] `shouldBe` 8.0
      P.perimeter [(P.Point 2 2), (P.Point 2 0), (P.Point 0 0), (P.Point 0 2)] `shouldBe` 8.0
      N.perimeter [(N.Point 3 3), (N.Point 2 0), (N.Point 0 1), (N.Point 0 2)] `shouldBe` 9.56062329783655
      P.perimeter [(P.Point 3 3), (P.Point 2 0), (P.Point 0 1), (P.Point 0 2)] `shouldBe` 9.56062329783655
    it "area" $ do
      N.doubleArea [(N.Point 1 1), (N.Point 1 0), (N.Point 0 0), (N.Point 0 1)] `shouldBe` 2
      P.doubleArea [(P.Point 1 1), (P.Point 1 0), (P.Point 0 0), (P.Point 0 1)] `shouldBe` 2
      N.doubleArea [(N.Point 2 2), (N.Point 2 0), (N.Point 0 0), (N.Point 0 2)] `shouldBe` 8
      P.doubleArea [(P.Point 2 2), (P.Point 2 0), (P.Point 0 0), (P.Point 0 2)] `shouldBe` 8
      N.doubleArea [(N.Point 3 3), (N.Point 2 0), (N.Point 0 1), (N.Point 0 2)] `shouldBe` 20
      P.doubleArea [(P.Point 3 3), (P.Point 2 0), (P.Point 0 1), (P.Point 0 2)] `shouldBe` 20