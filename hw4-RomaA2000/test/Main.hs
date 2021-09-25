module Main
  ( main,
  )
where

import Task1Spec
import Task2Spec
import Task7Spec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  t1 <- geometryTestsTree
  t2 <- mcTestsTree
  t3 <- fsTestsTree
  defaultMain $ testGroup "Tests" [t1, t2, t3]