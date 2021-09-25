module Block3_2Spec (semigroupMonoidTestsTree) where

import           Block3_2
import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Hspec

semigroupMonoidTestsTree :: IO TestTree
semigroupMonoidTestsTree = testSpec "Semigroup and monoid" semigroupMonoidTests

semigroupMonoidTests :: Spec
semigroupMonoidTests = do
  describe "NonEmpty" $
    it "semigroup rules for NonEmpty" $ do
      (0 :| []) <> (1 :| []) `shouldBe` (0 :| [1])
      (0 :| [1]) <> (2 :| [3]) `shouldBe` (0 :| [1, 2, 3])
      ((0 :| [1]) <> (2 :| [3])) <> (4 :| [5]) `shouldBe` (0 :| [1, 2, 3, 4, 5])
      (0 :| [1]) <> ((2 :| [3]) <> (4 :| [5])) `shouldBe` (0 :| [1, 2, 3, 4, 5])
      ((0 :| [0, 1, 1]) <> (2 :| [2, 3, 3])) <> (4 :| [4, 5, 5]) `shouldBe` (0 :| [0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5])
      (0 :| [0, 1, 1]) <> ((2 :| [2, 3, 3]) <> (4 :| [4, 5, 5])) `shouldBe` (0 :| [0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5])

  describe "ThisOrThat" $
    it "semigroup rules for ThisOrThat" $ do
      This 0 <> This 1 `shouldBe` (This 0 :: ThisOrThat Int Int)
      This 1 <> (This 2 <> This 3) `shouldBe` (This 1 :: ThisOrThat Int Int)
      (This 1 <> This 2) <> This 3 `shouldBe` (This 1 :: ThisOrThat Int Int)

      That 0 <> That 1 `shouldBe` (That 0 :: ThisOrThat Int Int)
      That 1 <> (That 2 <> That 3) `shouldBe` (That 1 :: ThisOrThat Int Int)
      (That 1 <> That 2) <> That 3 `shouldBe` (That 1 :: ThisOrThat Int Int)

      Both 0 0 <> Both 1 1 `shouldBe` (Both 0 0 :: ThisOrThat Int Int)
      Both 1 1 <> (Both 2 2 <> Both 3 3) `shouldBe` (Both 1 1 :: ThisOrThat Int Int)
      (Both 1 1 <> Both 2 2) <> Both 3 3 `shouldBe` (Both 1 1 :: ThisOrThat Int Int)

      This 0 <> Both 1 1 `shouldBe` (Both 0 1 :: ThisOrThat Int Int)
      That 0 <> Both 1 1 `shouldBe` (Both 1 0 :: ThisOrThat Int Int)
      This 0 <> That 1 `shouldBe` (Both 0 1 :: ThisOrThat Int Int)
      That 0 <> This 1 `shouldBe` (Both 1 0 :: ThisOrThat Int Int)

  describe "Name" $ do
    it "semigroup rules for Name" $ do
      (Name "n1") <> (Name "n2") `shouldBe` Name "n1.n2"
      (Name "n1") <> ((Name "n2") <> (Name "n3")) `shouldBe` Name "n1.n2.n3"
      ((Name "n1") <> (Name "n2")) <> (Name "n3") `shouldBe` Name "n1.n2.n3"
    it "monoid rules for Name" $ do
      (Name "n1") `mappend` (Name "n2") `shouldBe` Name "n1.n2"
      (Name "n1") `mappend` ((Name "n2") `mappend` (Name "n3")) `shouldBe` Name "n1.n2.n3"
      ((Name "n1") `mappend` (Name "n2")) `mappend` (Name "n3") `shouldBe` Name "n1.n2.n3"
      mempty `mappend` (Name "n2") `shouldBe` Name "n2"
      (Name "n1") `mappend` mempty `shouldBe` Name "n1"
      mempty `mappend` (Name "") `shouldBe` Name ""

  describe "Endo" $ do
    it "semigroup rules for Endo" $ do
       (getEndo ((Endo (+ 1)) <> (Endo (* 2)))) 1 `shouldBe` (getEndo (Endo ((+ 1) . (* 2)))) 1
       (getEndo ((Endo (+ 5)) <> ((Endo (* 2)) <> (Endo (+ 1))))) 2 `shouldBe` (getEndo (Endo ((+ 5) . (* 2) . (+ 1)))) 2
       (getEndo (((Endo (+ 5)) <> (Endo (* 2))) <> (Endo (+ 1)))) 4 `shouldBe` (getEndo (Endo ((+ 5) . (* 2) . (+ 1)))) 4
    it "monoid rules for Endo" $ do
       (getEndo ((Endo (+ 1)) `mappend` (Endo (* 2)))) 1 `shouldBe` (getEndo (Endo ((+ 1) . (* 2)))) 1
       (getEndo ((Endo (+ 5)) `mappend` ((Endo (* 2)) `mappend` (Endo (+ 1))))) 2 `shouldBe` (getEndo (Endo ((+ 5) . (* 2) . (+ 1)))) 2
       (getEndo (((Endo (+ 5)) `mappend` (Endo (* 2))) `mappend` (Endo (+ 1)))) 4 `shouldBe` (getEndo (Endo ((+ 5) . (* 2) . (+ 1)))) 4
       (getEndo ((Endo (+ 1)) `mappend` (Endo id))) 1 `shouldBe` (getEndo (Endo ((+ 1)))) 1
       (getEndo ((Endo id) `mappend` (Endo (* 2)))) 2 `shouldBe` (getEndo (Endo (* 2))) 2
       (getEndo ((Endo id) `mappend` (Endo id))) 2 `shouldBe` (getEndo (Endo id)) 2
