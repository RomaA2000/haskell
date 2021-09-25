module Block1_1Spec
  ( fSTestTree,
  )
where

import Control.Monad.Except (ExceptT, catchError, lift, runExceptT)
import Control.Monad.State (StateT, evalState, evalStateT, execState, execStateT, get, lift, put, runStateT)
import Data.List.NonEmpty (NonEmpty ((:|)), fromList, toList)
import qualified Data.Map as Map
import DirClasses
import System.IO.Error (tryIOError)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)
import Utils (FSError (..))

constructFileSystem :: FileSystem
constructFileSystem = do
  let fileData1 = "data"
  let fileName1 = "name"
  let file1 = File fileName1 True fileData1
  let fileElem1 = F file1

  let fileData2 = "data2 data2"
  let fileName2 = "name2"
  let file2 = File fileName2 True fileData2
  let fileElem2 = F file2

  let fileData3 = "data3 data3 data3"
  let fileName3 = "name3"
  let file3 = File fileName3 True fileData3
  let fileElem3 = F file3

  let fileMap2 = Map.fromList [(fileName2, fileElem2), (fileName3, fileElem3)]
  let dirName2 = "dir"
  let dir2 = Directory dirName2 True fileMap2
  let dirElem2 = D dir2

  let fileMap1 = Map.fromList [(fileName1, fileElem1), (dirName2, dirElem2)]
  let dir1 = Directory "$" True fileMap1
  let fs = FileSystem dir1
  fs

fSTestTree :: IO TestTree
fSTestTree = testSpec "Files Testing" fSTests

fSTests :: Spec
fSTests = do
  describe "cd" $ do
    it "cd in $" $
      (evalState (execStateT (runExceptT (cd "$")) "$") constructFileSystem) `shouldBe` "$"
    it "cd in ." $
      (evalState (execStateT (runExceptT (cd ".")) "$") constructFileSystem) `shouldBe` "$"
    it "cd in dir" $
      (evalState (execStateT (runExceptT (cd "dir")) "$") constructFileSystem) `shouldBe` "$/dir"
    it "cd in dir/../dir" $
      (evalState (execStateT (runExceptT (cd "dir/../dir")) "$") constructFileSystem) `shouldBe` "$/dir"
    it "cd in dir/../././dir" $
      (evalState (execStateT (runExceptT (cd "dir/../././dir")) "$") constructFileSystem) `shouldBe` "$/dir"
    it "cd .. in $" $
      (evalState (evalStateT (runExceptT (cd "..")) "$") constructFileSystem) `shouldBe` Left (PathFormatError "trying to go up from root")
  describe "ls" $ do
    it "ls in $" $
      (evalState (evalStateT (runExceptT (ls "$")) "$") constructFileSystem) `shouldBe` Right ["dir", "name"]
    it "ls in ." $
      (evalState (evalStateT (runExceptT (ls ".")) "$") constructFileSystem) `shouldBe` Right ["dir", "name"]
    it "ls in $/dir" $
      (evalState (evalStateT (runExceptT (ls "$/dir")) "$") constructFileSystem) `shouldBe` Right ["name2", "name3"]
    it "ls in dir" $
      (evalState (evalStateT (runExceptT (ls "dir")) "$") constructFileSystem) `shouldBe` Right ["name2", "name3"]
    it "ls in dir/../dir" $
      (evalState (evalStateT (runExceptT (ls "dir/../dir")) "$") constructFileSystem) `shouldBe` Right ["name2", "name3"]
    it "ls in dir/../././dir" $
      (evalState (evalStateT (runExceptT (ls "dir/../././dir")) "$") constructFileSystem) `shouldBe` Right ["name2", "name3"]
    it "ls in .." $
      (evalState (evalStateT (runExceptT (ls "..")) "$") constructFileSystem) `shouldBe` Left (PathFormatError "trying to go up from root")
  describe "info of file" $ do
    it "info of name" $
      (evalState (evalStateT (runExceptT (getFileInfoFromPath "name")) "$") constructFileSystem) `shouldBe` Right (4, ["Path: $/name", "Permissions: True"])
    it "info of dir/name2" $
      (evalState (evalStateT (runExceptT (getFileInfoFromPath "dir/name2")) "$") constructFileSystem) `shouldBe` Right (11, ["Path: $/dir/name2", "Permissions: True"])
    it "info of $/dir/name3" $
      (evalState (evalStateT (runExceptT (getFileInfoFromPath "dir/name3")) "$") constructFileSystem) `shouldBe` Right (17, ["Path: $/dir/name3", "Permissions: True"])
    it "info of $/dir/name4" $
      (evalState (evalStateT (runExceptT (getFileInfoFromPath "dir/name4")) "$") constructFileSystem) `shouldBe` Left (ChangingDirectoryError "no such file")
  describe "info of dir" $ do
    it "info of $" $
      (evalState (evalStateT (runExceptT (getInfoOfDir "$")) "$") constructFileSystem) `shouldBe` Right ["Path: $", "Permissions: True"]
    it "info of $/dir" $
      (evalState (evalStateT (runExceptT (getInfoOfDir "$/dir")) "$") constructFileSystem) `shouldBe` Right ["Path: $/dir", "Permissions: True"]
  describe "split on dirs or files" $ do
    it "split of dir ans name" $
      (evalState (evalStateT (runExceptT (splitOnDirsAndFiles "$" ["dir", "name"])) "$") constructFileSystem) `shouldBe` Right (["name"], ["dir"])
  describe "all info of directory" $ do
    it "all info of $" $
      (evalState (evalStateT (runExceptT (getDirInfoFromPath "$")) "$") constructFileSystem) `shouldBe` Right (3, 32, ["Path: $", "Permissions: True"])
    it "all info of dir" $
      (evalState (evalStateT (runExceptT (getDirInfoFromPath "dir")) "$") constructFileSystem) `shouldBe` Right (2, 28, ["Path: $/dir", "Permissions: True"])
  describe "find file in directory" $ do
    it "name in $" $
      (evalState (evalStateT (runExceptT (findFile "$" "name")) "$") constructFileSystem) `shouldBe` Right (Just "$/name")
    it "name2 in $" $
      (evalState (evalStateT (runExceptT (findFile "$" "name2")) "$") constructFileSystem) `shouldBe` Right (Just "$/dir/name2")
    it "name3 in $" $
      (evalState (evalStateT (runExceptT (findFile "$" "name3")) "$") constructFileSystem) `shouldBe` Right (Just "$/dir/name3")
    it "name2 in $/dir" $
      (evalState (evalStateT (runExceptT (findFile "$/dir" "name3")) "$") constructFileSystem) `shouldBe` Right (Just "$/dir/name3")
    it "name3 in $/dir" $
      (evalState (evalStateT (runExceptT (findFile "$/dir" "name3")) "$") constructFileSystem) `shouldBe` Right (Just "$/dir/name3")
    it "name3 in $" $
      (evalState (evalStateT (runExceptT (findFile "$" "name4")) "$") constructFileSystem) `shouldBe` Right Nothing
    it "dir in $" $
      (evalState (evalStateT (runExceptT (findFile "$" "dir")) "$") constructFileSystem) `shouldBe` Right Nothing
  describe "cat file in directory" $ do
    it "name in $" $
      (evalState (evalStateT (runExceptT (cat "name")) "$") constructFileSystem) `shouldBe` Right "data"
    it "name in dir" $
      (evalState (evalStateT (runExceptT (cat "name2")) "$/dir") constructFileSystem) `shouldBe` Right "data2 data2"
    it "name in dir" $
      (evalState (evalStateT (runExceptT (cat "name3")) "$/dir") constructFileSystem) `shouldBe` Right "data3 data3 data3"
    it "name in $/dir/./.././dir/" $
      (evalState (evalStateT (runExceptT (cat "name")) "$") constructFileSystem) `shouldBe` Right "data"
  describe "touch file in directory" $ do
    it "name4 in $ + ls" $
      (evalState (evalStateT (runExceptT (do {createFile "name4"; ls "."})) "$") constructFileSystem) `shouldBe` Right  ["dir", "name", "name4"]
    it "name4 in dir + ls" $
      (evalState (evalStateT (runExceptT (do {createFile "dir/name4"; ls "dir"})) "$") constructFileSystem) `shouldBe` Right  ["name2", "name3", "name4"]
    it "name4 in $/dir/../././dir + ls" $
      (evalState (evalStateT (runExceptT (do {createFile "$/dir/../././dir/name4"; ls "dir"})) "$") constructFileSystem) `shouldBe` Right  ["name2", "name3", "name4"]
    it "name4 in $/./dir/../././dir/name2 + ls" $
      (evalState (evalStateT (runExceptT (do {createFile "./dir/../././dir/name2"; ls "dir"})) "$") constructFileSystem) `shouldBe` Left (CreatingFileError "name already exist")
    it "name4 in $/./dir/../././dir/dir2 + ls" $
      (evalState (evalStateT (runExceptT (do {createFile "$/./dir/../././dir/dir2"; ls "dir"})) "$") constructFileSystem) `shouldBe` Right ["dir2", "name2", "name3"]
    it "name4 in $ + cat" $
      (evalState (evalStateT (runExceptT (do {createFile "name4"; cat "name4"})) "$") constructFileSystem) `shouldBe` Right ""
    it "name4 in dir + cat" $
      (evalState (evalStateT (runExceptT (do {createFile "dir/name4"; cat "dir/name4"})) "$") constructFileSystem) `shouldBe` Right ""
    it "name4 in $ + findf" $
      (evalState (evalStateT (runExceptT (do {createFile "name4"; findFile "$" "name4"})) "$") constructFileSystem) `shouldBe` Right (Just "$/name4")
    it "name4 in dir + findf" $
      (evalState (evalStateT (runExceptT (do {createFile "dir/name4"; findFile "$" "name4"})) "$") constructFileSystem) `shouldBe` Right (Just "$/dir/name4")
    it "name4 and name5 in $ + ls" $
      (evalState (evalStateT (runExceptT (do {createFile "name4"; createFile "name5"; ls "."})) "$") constructFileSystem) `shouldBe` Right ["dir", "name", "name4", "name5"]
    it "name4 and name5 in dir + ls" $
      (evalState (evalStateT (runExceptT (do {createFile "dir/name4"; createFile "dir/name5"; ls "dir"})) "$") constructFileSystem) `shouldBe` Right ["name2", "name3", "name4", "name5"]
    it "name4 and name5 in $ + cat" $
      (evalState (evalStateT (runExceptT (do {createFile "name4";  createFile "name5"; cat "name5"})) "$") constructFileSystem) `shouldBe` Right ""
    it "name4 and name5 in dir + cat" $
      (evalState (evalStateT (runExceptT (do {createFile "dir/name4"; createFile "dir/name5"; cat "dir/name5"})) "$") constructFileSystem) `shouldBe` Right ""
    it "name4 and name5 in $ + findf" $
      (evalState (evalStateT (runExceptT (do {createFile "name4"; createFile "name5"; findFile "$" "name5"})) "$") constructFileSystem) `shouldBe` Right (Just "$/name5")
    it "name4 and name5 in dir + findf" $
      (evalState (evalStateT (runExceptT (do {createFile "dir/name4"; createFile "dir/name5"; findFile "$" "name5"})) "$") constructFileSystem) `shouldBe` Right (Just "$/dir/name5")
  describe "mkdir directory in directory" $ do
    it "name4 in $ + ls" $
      (evalState (evalStateT (runExceptT (do {mkdir "name4"; ls "."})) "$") constructFileSystem) `shouldBe` Right ["dir", "name", "name4"]
    it "name4 in $/dir + ls" $
      (evalState (evalStateT (runExceptT (do {mkdir "dir/name4"; ls "dir"})) "$") constructFileSystem) `shouldBe` Right ["name2", "name3", "name4"]
    it "dir2 in $/dir + cd" $
      (evalState (evalStateT (runExceptT (do {mkdir "dir/dir2"; cd "dir/dir2"; ls "."})) "$") constructFileSystem) `shouldBe` Right []
    it "dir2 in $ + ls" $
      (evalState (evalStateT (runExceptT (do {mkdir "dir2"; cd "dir2"; ls "."})) "$") constructFileSystem) `shouldBe` Right []
    it "wow in $/dir + ls" $
      (evalState (evalStateT (runExceptT (do {mkdir "dir2"; cd "dir2"; createFile "wow"; ls "."})) "$") constructFileSystem) `shouldBe` Right ["wow"]
    it "many dirs" $
      (evalState (evalStateT (runExceptT (do {mkdir "dir2"; cd "dir2"; mkdir "dir3"; cd "dir3"; ls "."})) "$") constructFileSystem) `shouldBe` Right []
    it "dir2 in $/dir + cd" $
      (evalState (evalStateT (runExceptT (do {mkdir "dir2"; cd "dir2";  cd "dir3";  cd "dir2"; ls "."})) "$") constructFileSystem) `shouldBe` Left (ChangingDirectoryError "no such directory")
