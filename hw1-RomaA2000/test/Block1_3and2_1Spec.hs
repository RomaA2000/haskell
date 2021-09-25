module Block1_3and2_1Spec (treeTestsTree) where

import           Block1_3and2_1
import           Data.Foldable      hiding (find)
import           Data.List          (sort)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Hspec

treeTestsTree :: IO TestTree
treeTestsTree = testSpec "Tree" treeTests

treeTests :: Spec
treeTests = do
  describe "toList and fromList" $
    it "checking converting to and from list" $ do
      (fromList [] ::Tree Int) `shouldBe` (Leaf ::Tree Int)
      (fromList [1] ::Tree Int) `shouldBe` ((Node (1 :| []) Leaf Leaf) ::Tree Int)
      (fromList (1 : [2]) ::Tree Int) `shouldBe` ((Node (1 :| []) Leaf (Node (2 :| []) Leaf Leaf)) ::Tree Int)
      (toList (fromList [1, 3, 5, 8, 9] ::Tree Int)) `shouldBe` [1, 3, 5, 8, 9]
      (toList (fromList [8, 9, 1] ::Tree Int)) `shouldBe` [1, 8, 9]
      (toList (fromList [1, 9] ::Tree Int)) `shouldBe` [1, 9]
      (toList (fromList [9, 20, 0] ::Tree Int)) `shouldBe` [0, 9, 20]
      (toList (fromList [1, 0, -1, -4] ::Tree Int)) `shouldBe` [-4, -1, 0, 1]
      (toList (fromList [-1, -9] ::Tree Int)) `shouldBe` [-9, -1]
      (toList (fromList [1, 0, 0, -1, -1, -4] ::Tree Int)) `shouldBe` [-4, -1, -1, 0, 0, 1]
      (toList (fromList [-1, 6, 6, 6, -9] ::Tree Int)) `shouldBe` [-9, -1, 6, 6, 6]

  describe "toList and fromList big" $
    it "checking composition toList and fromList" $ do
      (toList . fromList) [-1, 6, 6, 6, -9, 2, 1, 3, 3, -5, 12, 3, 4, 5, -1, -10, 9 , 12] `shouldBe` sort [-1, 6, 6, 6, -9, 2, 1, 3, 3, -5, 12, 3, 4, 5, -1, -10, 9 , 12]
      (toList . fromList) [-1, 6, 6, 6, 3, 4, 5, -1, -10, 9 , 12] `shouldBe` sort [-1, 6, 6, 6, 3, 4, 5, -1, -10, 9 , 12]
      (toList . fromList) [-1, 3, -1, -14, -17,-20, 20, 30, 40, 4, 5, -1, -10, 9 , 12] `shouldBe` sort [-1, 3, -1, -14, -17,-20, 20, 30, 40, 4, 5, -1, -10, 9 , 12]

  describe "empty" $
    it "checking if tree in empty" $ do
      isEmpty Leaf `shouldBe` True
      isEmpty (Node (0 :| []) Leaf Leaf) `shouldBe` False
      isEmpty ((Node (1 :| []) Leaf (Node (2 :| []) Leaf Leaf)) ::Tree Int) `shouldBe` False
      isEmpty (fromList [-1, -9] ::Tree Int) `shouldBe` False
      isEmpty (fromList [] ::Tree Int) `shouldBe` True
      isEmpty (fromList [1, 0, -1, -4] ::Tree Int) `shouldBe` False

  describe "size" $
    it "checking size of the tree" $ do
      size Leaf `shouldBe` 0
      size (Node (0 :| []) Leaf Leaf) `shouldBe` 1
      size ((Node (1 :| []) Leaf (Node (2 :| []) Leaf Leaf)) ::Tree Int) `shouldBe` 2
      size (fromList [-1, -9] ::Tree Int) `shouldBe` 2
      size (fromList [] ::Tree Int) `shouldBe` 0
      size (fromList [1, 0, -1, -4] ::Tree Int) `shouldBe` 4
      size ((Node (1 :| []) Leaf (Node (2 :| [2, 2]) Leaf Leaf)) ::Tree Int) `shouldBe` 4

  describe "find" $
    it "checking find in the tree" $ do
      find Leaf (0 ::Int) `shouldBe` False
      find (Node (0 :| []) Leaf Leaf) (0 ::Int) `shouldBe` True
      find ((Node (0 :| []) Leaf (Node (2 :| []) Leaf Leaf)) ::Tree Int) (2 ::Int) `shouldBe` True
      find (fromList [-1, -9, -8, 0, 1] ::Tree Int) (-1 ::Int) `shouldBe` True
      find (fromList [] ::Tree Int) (0 ::Int) `shouldBe` False
      find (fromList [5, 10, -11, -4] ::Tree Int) (-4 ::Int) `shouldBe` True
      find ((Node (0 :| []) Leaf (Node (2 :| [2, 2]) Leaf Leaf)) ::Tree Int) (2 ::Int) `shouldBe` True

  describe "insert" $
    it "checking insert in the tree" $ do
      toList (insert (Leaf) (0 ::Int)) `shouldBe` [0]
      toList (insert (fromList [-1, -9, -8, 0, 1] ::Tree Int) (5 ::Int)) `shouldBe` [-9, -8, -1, 0, 1, 5]
      toList (insert (fromList [5, 10, -11, -4, 6] ::Tree Int) (6 ::Int)) `shouldBe` [-11, -4, 5, 6, 6, 10]
      toList (insert (fromList [10, -11, -4, 6] ::Tree Int) (5 ::Int)) `shouldBe` [-11, -4, 5, 6, 10]
      toList (insert (fromList [-1, -9, -8, 1] ::Tree Int) (0 ::Int)) `shouldBe` [-9, -8, -1, 0, 1]

  describe "remove" $
    it "checking remove from the tree" $ do
      toList (remove (Leaf) (0 ::Int)) `shouldBe` []
      toList (remove (fromList [-1, -9, -8, 0, 1] ::Tree Int) (0 ::Int)) `shouldBe` [-9, -8, -1, 1]
      toList (remove (fromList [5, 10, -11, -4, 6] ::Tree Int) (6 ::Int)) `shouldBe` [-11, -4, 5, 10]
      toList (remove (fromList [10, -11, -4, 6] ::Tree Int) (-4 ::Int)) `shouldBe` [-11, 6, 10]
      toList (remove (fromList [-1, -9, -8, 1, 10, 12, 14, -5] ::Tree Int) (1 ::Int)) `shouldBe` [-9, -8, -5, -1, 10, 12, 14]
      toList (remove (fromList [-1, -9, -8, 1, 10, 12, 14, -5] ::Tree Int) (-1 ::Int)) `shouldBe` [-9, -8, -5, 1, 10, 12, 14]
      toList (remove (fromList [-1, -9, -8, 1, 10, 12, 14, -5] ::Tree Int) (-5 ::Int)) `shouldBe` [-9, -8, -1, 1, 10, 12, 14]
      toList (remove (fromList [-1, -9, -8, 1, 10, 12, 14, -5] ::Tree Int) (-8 ::Int)) `shouldBe` [-9, -5, -1, 1, 10, 12, 14]
      toList (remove (fromList [-1, -9, -8, 1, 10, 12, 14, -5] ::Tree Int) (10 ::Int)) `shouldBe` [-9, -8, -5, -1, 1, 12, 14]
      toList (remove (fromList [-1, -9, -8, 1, 10, 12, 14, -5] ::Tree Int) (12 ::Int)) `shouldBe` [-9, -8, -5, -1, 1, 10, 14]
      toList (remove (fromList [-1, -9, -8, 1, 10, 12, 14, 14, -5] ::Tree Int) (14 ::Int)) `shouldBe` [-9, -8, -5, -1, 1, 10, 12, 14]

  describe "foldr" $
    it "checking foldr for the tree" $ do
      foldr (+) 0 (Leaf) `shouldBe` 0
      foldr (+) 0 (fromList [-1, -9] ::Tree Int) `shouldBe` -10
      foldr (+) 0 (fromList [2, -2, 1, 2] ::Tree Int) `shouldBe` 3
      foldr (*) 1 (fromList [1, -2, -2, 1] ::Tree Int) `shouldBe` 4
      foldr (*) 1 (fromList [1, -2, -2, 2, 2, 0, 1] ::Tree Int) `shouldBe` 0
