module Block2_1PropSpec
        ( treeFoldableTestTree
        ) where

import Data.Foldable (toList)
import Data.List (sort)
import Test.Hspec

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Block1_3and2_1 (fromList)

treeFoldableTestTree :: IO TestTree
treeFoldableTestTree = return $
  testGroup "Tree prop"
  [testProperty "toList . fromList == sort" propIntFold]

genIntList :: Gen [Int]
genIntList = Gen.list (Range.linear 0 1000) Gen.enumBounded

propIntFold :: Property
propIntFold = property $ do
  lInt <- forAll genIntList
  (toList $ fromList lInt) === (sort lInt)