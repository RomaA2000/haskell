module Main
  ( main,
  )
where

import Block1_1Spec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  b1_1 <- fSTestTree
  defaultMain $ testGroup "Tests" [b1_1]
