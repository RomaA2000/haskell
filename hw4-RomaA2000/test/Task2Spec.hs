module Task2Spec
    (mcTestsTree
    ) where

import Task2
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

mcTestsTree :: IO TestTree
mcTestsTree = testSpec "Monte-Carlo" mcTests

mcTests :: Spec
mcTests = do
  describe "monte-carlo calculation" $ do
    it "cos" $ do
      (0.01 > (abs ((-) (calculator 0 1 cos 100000) 0.84))) `shouldBe` True
      (0.01 > (abs ((-) (calculatorP 0 1 cos 100000) 0.84))) `shouldBe` True
      (0.005 > (abs ((-) (calculator 0 0.5 cos 100000) 0.479))) `shouldBe` True
      (0.005 > (abs ((-) (calculatorP 0 0.5 cos 100000) 0.479))) `shouldBe` True
    it "sin" $ do
      (0.01 > (abs ((-) (calculator 0 1 sin 100000) 0.459))) `shouldBe` True
      (0.01 > (abs ((-) (calculatorP 0 1 sin 100000) 0.459))) `shouldBe` True
      (0.005 > (abs ((-) (calculator 0 0.5 sin 100000) 0.122))) `shouldBe` True
      (0.005 > (abs ((-) (calculatorP 0 0.5 sin 100000) 0.122))) `shouldBe` True