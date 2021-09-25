module Block1_2PropSpec
        ( natPropTestTree
        ) where


import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Hspec

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Block1_2

natPropTestTree :: IO TestTree
natPropTestTree = return $
  testGroup "Nat prop"
  [testProperty "add" propIntAdd,
   testProperty "mod" propIntMod,
   testProperty "mul" propIntMul,
   testProperty "div" propIntDiv]

genInteger :: Gen Integer
genInteger = Gen.integral (Range.linear 0 1000)

propIntAdd :: Property
propIntAdd = property $ do
  first <- forAll genInteger
  second <- forAll genInteger
  (intFromNat ((+) (fromInteger first) (fromInteger second))) === ((+) first second)

propIntMod :: Property
propIntMod = property $ do
  first <- forAll genInteger
  second <- forAll genInteger
  (intFromNat ((modNat) (fromInteger first) (fromInteger (second + 1)))) === ((mod) first (second + 1))

propIntDiv :: Property
propIntDiv = property $ do
  first <- forAll genInteger
  second <- forAll genInteger
  (intFromNat ((divNat) (fromInteger first) (fromInteger (second + 1)))) === ((div) first (second + 1))

propIntMul :: Property
propIntMul = property $ do
  first <- forAll genInteger
  second <- forAll genInteger
  (intFromNat ((*) (fromInteger first) (fromInteger second))) === ((*) first second)


