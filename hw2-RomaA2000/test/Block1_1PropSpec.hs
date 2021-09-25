module Block1_1PropSpec
    ( stringPropSumTestsTree
    ) where

import           Block1_1
import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.QuickCheck (property)
import           Data.Foldable

stringPropSumTestsTree :: IO TestTree
stringPropSumTestsTree = testSpec "Property String Sum" stringSumTests

stringSumTests :: Spec
stringSumTests = do
  describe "Property Summing" $ do
    it "property summing correct string" $ do
      property checker
        where
          checker :: [Int] -> Bool
          checker list = maybeSum (foldl' (\val now -> show now ++ " \n \t" ++ val) "" list) == Just (sum list)