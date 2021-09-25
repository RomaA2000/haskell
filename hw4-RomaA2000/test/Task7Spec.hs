module Task7Spec
    (fsTestsTree
    ) where

import Task6
import Task7
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import Lens.Micro
import Data.Maybe (fromJust)

fs = Dir "$" [File "ff", Dir "fd" [File "fff"], Dir "sd" [File "sff"], File "sf"]

fsTestsTree :: IO TestTree
fsTestsTree = testSpec "File-system" fsTests

fsTests :: Spec
fsTests = do
  describe "file systems" $ do
    it "content of fs" $ do
      (fs ^. contents) `shouldBe` (contents' fs)
    it "_content of fs" $ do
      (fs ^. _contents) `shouldBe` (contents' fs)
    it "name of fs" $ do
      (fs ^. name) `shouldBe` (name' fs)
    it "_dir of fs" $ do
      (has _dir fs) `shouldBe` True
    it "_file of fs" $ do
      (has _file fs) `shouldBe` False
  describe "cd ls file fs" $ do
    it "cd in fs" $ do
      fs ^? cd "fd" . name `shouldBe` Just ("fd" :: FilePath)
      fs ^? cd "ll" `shouldBe` Nothing
      fs ^? cd "sd" . name `shouldBe` Just ("sd" :: FilePath)
    it "ls in fs" $ do
      (fs ^.. ls') `shouldBe` ["ff", "fd", "sd", "sf"]
      ((fromJust (fs ^? cd "fd")) ^.. ls') `shouldBe` ["fff"]
      ((fromJust (fs ^? cd "sd")) ^.. ls') `shouldBe` ["sff"]
    it "file in fs" $ do
      fs ^? file' "ff" `shouldBe` Just (File "ff")
      fs ^? file' "sf" `shouldBe` Just (File "sf")
    it "file and cd in fs" $ do
      fs ^? cd "fd" . file' "fff" `shouldBe` Just (File "fff")
      fs ^? cd "sd" . file' "sff" `shouldBe` Just (File "sff")