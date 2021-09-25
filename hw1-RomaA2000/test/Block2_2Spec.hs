module Block2_2Spec (splitJoinTestsTree) where

import           Block2_2
import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Hspec
import           Data.List.NonEmpty (NonEmpty ((:|)))

castomId c = (joinWith c) . (splitOn c)

splitJoinTestsTree :: IO TestTree
splitJoinTestsTree = testSpec "Split and join" splitJoinTests

splitJoinTests :: Spec
splitJoinTests = do
  describe "splitOn" $
    it "splits the list with given element" $ do
      splitOn '1' "01010" `shouldBe` ("0" :| ["0", "0"])
      splitOn 1 [0, 0, 1, 0, 0, 1, 2, 2, 1, 1] `shouldBe` ([0, 0] :| [[0, 0], [2, 2], [], []])
      splitOn '/' "string/to/be" `shouldBe` ("string" :| ["to", "be"])
      splitOn '/' "/string/to/be/" `shouldBe` ("" :| ["string", "to", "be", ""])

  describe "joinWith" $
    it "joins lists with given element" $ do
      joinWith '1' ("0" :| ["2", "0"]) `shouldBe` "01210"
      joinWith 1 ([3, 3] :| [[0, 0], [2, 2]]) `shouldBe` [3, 3, 1, 0, 0, 1, 2, 2]
      joinWith '/' ("join" :| ["this", "now"]) `shouldBe` "join/this/now"
      joinWith '/' ("join" :| ["this", "now", ""]) `shouldBe` "join/this/now/"
      joinWith '/' ("" :| ["join", "this", "now"]) `shouldBe` "/join/this/now"

  describe "joinWith and splitOn big" $
    it "splits the list with given element and then joins it back" $ do
      (castomId '1') "01010" `shouldBe` id "01010"
      (castomId '1') "213819238103281023981203" `shouldBe` id "213819238103281023981203"
      (castomId '\\') "\\weq\\we\\qweq\\weq\\weq\\weqwe\\werwer\\we\\r\\\"" `shouldBe` id "\\weq\\we\\qweq\\weq\\weq\\weqwe\\werwer\\we\\r\\\""
      (castomId '#') "hghghg##hjg#GHJ#GJ#GJ#G#HG#G#JHG#JH##HG#bnb#" `shouldBe` id "hghghg##hjg#GHJ#GJ#GJ#G#HG#G#JHG#JH##HG#bnb#"
      (castomId '0') "01239102218426532427341024" `shouldBe` id "01239102218426532427341024"
      (castomId '1') "01239102218426532427341024" `shouldBe` id "01239102218426532427341024"
      (castomId '2') "01239102218426532427341024" `shouldBe` id "01239102218426532427341024"
      (castomId '3') "01239102218426532427341024" `shouldBe` id "01239102218426532427341024"
      (castomId '4') "01239102218426532427341024" `shouldBe` id "01239102218426532427341024"
