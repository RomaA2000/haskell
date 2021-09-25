module Main
  ( main
  ) where


import Block1_1Spec
import Block1_2Spec
import Block1_3and2_1Spec
import Block2_2Spec
import Block2_2PropSpec
import Block3_1Spec
import Block3_2Spec
import Block1_2PropSpec
import Block2_1PropSpec


import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  b1_1 <- dayTestsTree
  b1_2 <- natTestsTree
  b1_2prop <- natPropTestTree
  b1_3and2_1 <- treeTestsTree
  b2_1prop <- treeFoldableTestTree
  b2_2 <- splitJoinTestsTree
  b2_2prop <- splitJoinPropTestTree
  b3_1 <- maybeEitherConcatTestsTree
  b3_2 <- semigroupMonoidTestsTree
  defaultMain $ testGroup "Tests" [b1_1, b1_2, b1_2prop, b1_3and2_1, b2_1prop,
                                   b2_2, b2_2prop, b3_1, b3_2]
