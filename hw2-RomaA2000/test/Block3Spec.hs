module Block3Spec
    ( parserTestsTree
    ) where

import           Block3_1
import           Block3_2
import           Block3_3
import           Block3_4
import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Hspec
import           Control.Applicative (Alternative (..))

parserTestsTree :: IO TestTree
parserTestsTree = testSpec "Parser" parserTests

parserTests :: Spec
parserTests = do
  describe "Base Parser" $ do
    it "ok parsers" $ do
      runParser ok "asdasd" `shouldBe` Just ((), "asdasd")
      runParser ok "qweqwr" `shouldBe` Just ((), "qweqwr")

    it "eof parsers" $ do
      runParser eof "" `shouldBe` Just ((), "")
      runParser eof "qweqwr" `shouldBe` Nothing
      runParser eof "afjahf" `shouldBe` Nothing

    it "satisfy parsers" $ do
      runParser (satisfy (== 'e')) "efjahf" `shouldBe` Just ('e', "fjahf")
      runParser (satisfy (== 'a')) "afjahf" `shouldBe` Just ('a', "fjahf")
      runParser (satisfy (== 'd')) "d" `shouldBe` Just ('d', "")
      runParser (satisfy (== 'r')) "rafjahf" `shouldBe` Just ('r', "afjahf")
      runParser (satisfy (== 'f')) "dfjahf" `shouldBe` Nothing

    it "stream parsers" $ do
      runParser (stream "123") "123aba" `shouldBe` Just ("123", "aba")
      runParser (stream "1298") "1298abacaba" `shouldBe` Just ("1298", "abacaba")
      runParser (stream "shjs") "shjscaba" `shouldBe` Just ("shjs", "caba")
      runParser (stream "hlkr") "hlklfklf" `shouldBe` Nothing

    it "cbs parsers" $ do
      runParser cBSParser "" `shouldBe` Just (Empty, "")
      runParser cBSParser "()" `shouldBe` Just (Concat (Surround Empty) Empty, "")
      runParser cBSParser "()()" `shouldBe` Just (Concat (Surround Empty) (Concat (Surround Empty) Empty), "")
      runParser cBSParser "()()(" `shouldBe` Nothing

    it "applicative and fmap" $ do
      runParser ((fmap (++) (stream "e")) <*> (stream "f")) "efjahf" `shouldBe` Just ("ef", "jahf")
      runParser ((fmap (++) (stream "e")) <*> (stream "f")) "ef" `shouldBe` Just ("ef", "")
      runParser ((fmap (++) (stream "e")) <*> (stream "f")) "efjahf" `shouldBe` Just ("ef", "jahf")
      runParser ((fmap (++) (stream "ert")) <*> (stream "f")) "ertfjahf" `shouldBe` Just ("ertf", "jahf")
      runParser ((fmap (++) (stream "e")) <*> (stream "f")) "efd" `shouldBe` Just ("ef", "d")
      runParser ((fmap (++) (stream "ert")) <*> (stream "f")) "erdtfjahf" `shouldBe` Nothing

    it "monad and alternative" $ do
      runParser ((<|>) (stream "e") (stream "f")) "fr" `shouldBe` Just ("f", "r")
      runParser ((<|>) (stream "e") (stream "f")) "efr" `shouldBe` Just ("e", "fr")
      runParser ((pure 1) >>= (\x -> return (x * 10))) "" `shouldBe` Just (10, "")
      runParser ((pure 0) >>= (\x -> return (x + 1))) "" `shouldBe` Just (1, "")
      runParser ((<|>) (stream "e") (stream "f")) "aefr" `shouldBe` Nothing

    it "int parsers" $ do
      runParser parseIntegerEof "1" `shouldBe` Just (1 :: Integer, "")
      runParser parseIntegerEof "-1321" `shouldBe` Just ((-1321) :: Integer, "")
      runParser parseIntegerEof "-819238" `shouldBe` Just ((-819238) :: Integer, "")
      runParser parseIntegerEof "32178" `shouldBe` Just (32178 :: Integer, "")
      runParser parseIntegerEof "-a1" `shouldBe` Nothing
      runParser parseIntegerEof "-2j2" `shouldBe` Nothing

    it "int list parsers" $ do
      runParser parseListsIntEof "2,\n 1,\t+10 \n\t , 3,\n5,-7, 2" `shouldBe` Just ([[1, 10], [5, -7, 2]], "")
      runParser parseListsIntEof "4,\n\t 3,-3,\t1,3 \t , 1,5\n,1,\n\t 2" `shouldBe` Just ([[3, -3, 1, 3], [5], [2]], "")
      runParser parseListsIntEof "1,\n-10  ,\n\t 1,5,2, \n\t2,2" `shouldBe` Just ([[-10], [5], [2, 2]], "")
      runParser parseListsIntEof " " `shouldBe` Just ([], "")
      runParser parseListsIntEof "2,\n 1,+10 ,\n\t 1, \t1, 2,\n1,1 \t , 3,5,\n\t-7, 2" `shouldBe` Just ([[1, 10], [1], [1,1], [5, -7, 2]], "")
      runParser parseListsIntEof "2,\n 1,\t+10 \n\t , -3,\n5,-7, 2" `shouldBe` Nothing
      runParser parseListsIntEof "2,\n 1,\t+10 \n\t, , 3,\n5,-7, 2" `shouldBe` Nothing
      runParser parseListsIntEof "2,\n 1,\t+10 \n\t  5, 3,\n5,-7, 2" `shouldBe` Nothing
