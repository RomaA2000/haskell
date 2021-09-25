module Block2_2PropSpec
        ( splitJoinPropTestTree
        ) where

import Test.Hspec

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Block2_2

splitJoinPropTestTree :: IO TestTree
splitJoinPropTestTree = return $
  testGroup "Split join prop"
  [testProperty "joinWith . splitOn == id" propIntSplitJoin]

genInt :: Gen Int
genInt = Gen.int (Range.linear 0 1000)

genIntList :: Gen [Int]
genIntList = Gen.list (Range.linear 0 1000) Gen.enumBounded

propIntSplitJoin :: Property
propIntSplitJoin = property $ do
  lInt <- forAll genIntList
  s <- forAll genInt
  (joinWith s (splitOn s lInt)) === (id lInt)