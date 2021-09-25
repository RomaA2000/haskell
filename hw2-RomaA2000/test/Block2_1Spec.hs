module Block2_1Spec
    (exprEvalTestsTree
    ) where

import           Block2_1
import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Hspec

exprEvalTestsTree :: IO TestTree
exprEvalTestsTree = testSpec "Eval expr" exprEvalTests

makeAdd e1 e2 = (NoExcept (ExprNoExcept ((+) :: Int -> Int -> Int) '+' e1 e2))
makeSub e1 e2 = (NoExcept (ExprNoExcept ((-) :: Int -> Int -> Int) '-' e1 e2))
makeMul e1 e2 = (NoExcept (ExprNoExcept ((*) :: Int -> Int -> Int) '*' e1 e2))

exprEvalTests :: Spec
exprEvalTests = do
  describe "No errors" $ do
    it "summing" $ do
      eval (makeAdd (Const 2) (Const 1)) `shouldBe` Right 3
      eval (makeAdd (Const 12) (Const 112)) `shouldBe` Right 124
      eval (makeAdd (Const (-20)) (Const 1)) `shouldBe` Right (-19)
      eval (makeAdd (Const 123) (Const 9)) `shouldBe` Right 132
      eval (makeAdd (Const 21) (Const (-21))) `shouldBe` Right 0
      eval (makeAdd (Const (-2)) (Const (-1))) `shouldBe` Right (-3)

    it "multiplication" $ do
      eval (makeMul (Const 2) (Const 1)) `shouldBe` Right 2
      eval (makeMul (Const 12) (Const 12)) `shouldBe` Right 144
      eval (makeMul (Const (-20)) (Const 1)) `shouldBe` Right (-20)
      eval (makeMul (Const 123) (Const 9)) `shouldBe` Right 1107
      eval (makeMul (Const 21) (Const (-21))) `shouldBe` Right (-441)
      eval (makeMul (Const (-2)) (Const (-1))) `shouldBe` Right 2

    it "subtraction" $ do
      eval (makeSub (Const 2) (Const 1)) `shouldBe` Right 1
      eval (makeSub (Const (-20)) (Const (-1))) `shouldBe` Right (-19)
      eval (makeSub (Const 221) (Const 21)) `shouldBe` Right 200
      eval (makeSub (Const 1902) (Const 0)) `shouldBe` Right 1902
      eval (makeSub (Const 2) (Const (-1))) `shouldBe` Right 3

    it "power" $ do
      eval (Power (Const 2) (Const 1)) `shouldBe` Right 2
      eval (Power (Const (20)) (Const (1))) `shouldBe` Right 20
      eval (Power (Const 2) (Const 4)) `shouldBe` Right 16
      eval (Power (Const 0) (Const 0)) `shouldBe` Right 1
      eval (Power (Const 3) (Const (2))) `shouldBe` Right 9

    it "division" $ do
      eval (Divide (Const 2) (Const 1)) `shouldBe` Right 2
      eval (Divide (Const (-20)) (Const (-1))) `shouldBe` Right (20)
      eval (Divide (Const 210) (Const 21)) `shouldBe` Right 10
      eval (Divide (Const 1902) (Const 1)) `shouldBe` Right 1902
      eval (Divide (Const 2) (Const (-1))) `shouldBe` Right (-2)

    it "combination" $ do
      eval (Divide (Power (makeAdd (Const 2) (Const 0)) (Const 1)) (makeSub (makeMul (Const 2) (Const 1)) (Const 1))) `shouldBe` Right 2
      eval (makeSub (Const 11) (Divide (makeMul (Const 21) (Const 10)) (Const 21))) `shouldBe` Right 1

  describe "Some errors" $ do
    it "division" $ do
      eval (Divide (Const 2) (Const 0)) `shouldBe` Left DivisionByZero
      eval (Divide (Const (-20)) (Const (0))) `shouldBe` Left DivisionByZero
      eval (Divide (Const 210) (Const 0)) `shouldBe` Left DivisionByZero
      eval (Divide (Const 1902) (Const 0)) `shouldBe` Left DivisionByZero
      eval (Divide (Const 2) (Const (0))) `shouldBe` Left DivisionByZero

    it "power" $ do
      eval (Power (Const 2) (Const (-11))) `shouldBe` Left NegativeExponent
      eval (Power (Const (20)) (Const (-12))) `shouldBe` Left NegativeExponent
      eval (Power (Const 2) (Const (-12))) `shouldBe` Left NegativeExponent
      eval (Power (Const 0) (Const (-211))) `shouldBe` Left NegativeExponent
      eval (Power (Const 3) (Const (-211))) `shouldBe` Left NegativeExponent