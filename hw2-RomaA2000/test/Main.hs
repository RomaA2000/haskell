module Main
  ( main
  ) where

import           Test.Tasty (defaultMain, testGroup)
import           Block1_1Spec
import           Block1_1PropSpec
import           Block2_1Spec
import           Block3Spec

main :: IO ()
main = do
  b1_1 <- stringSumTestsTree
  b1_1Prop <- stringPropSumTestsTree
  b2_1 <- exprEvalTestsTree
  b3 <- parserTestsTree
  defaultMain $ testGroup "Tests" [b1_1, b1_1Prop, b2_1, b3]