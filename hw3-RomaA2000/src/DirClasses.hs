{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-
Module DirClasses:
Function for File System
-}
module DirClasses
  ( -- * The 'FSInterface' implementation
    FSMonad,

    -- * The 'File' data
    File (..),

    -- * The 'Directory' data
    Directory (..),

    -- * The 'FSElement' data
    FSElement (..),

    -- * The 'FileSystem' data
    FileSystem (..),

    -- * The 'FSInterface' data
    FSInterface (..),
  )
where

import Control.Monad.Except (lift, throwError)
import Control.Monad.State (State, get, put)
import Data.List.NonEmpty (toList)
import qualified Data.Map as Map
import FSInterface (Command, FSInterface (..))
import Utils (FSError (..), joinWith, splitOn)

-- | Type 'FSMonad' represents File System
type FSMonad = State FileSystem

-- | Data 'File' represents files in File System
data File = File
  { getFileName :: FilePath,
    getFilePermissions :: Bool,
    getData :: String
  }
  deriving (Show, Eq)

-- | Data 'Directory' represents dirs in File System
data Directory = Directory
  { getDirectoryName :: FilePath,
    getDirectoryPermissions :: Bool,
    getItems :: Map.Map FilePath FSElement
  }
  deriving (Show, Eq)

-- | Data 'FSElement' represents element of File System
data FSElement
  = F File
  | D Directory
  deriving (Show, Eq)

-- | Data 'FileSystem' represents File System
data FileSystem = FileSystem
  { getRoot :: Directory
  }
  deriving (Show, Eq)

-- | Instance 'FSInterface' creates File System for 'FSMonad'
instance FSInterface FSMonad where
  -- | Function 'normalizeDirWithAbs' returns absolute path from two provided
  normalizeDirWithAbs :: FilePath -> FilePath -> Command FSMonad FilePath
  normalizeDirWithAbs path pathNew = do
    let pathList = filePathToList pathNew
    let inputPathList = filePathToList path
    let result = if checkRelative inputPathList then (pathList ++ inputPathList) else inputPathList
    resultNormal <- normalizePath result
    return $ listToFilepath resultNormal

  -- | Function 'changeDir' changes directory to provided absolute
  changeDir :: FilePath -> Command FSMonad ()
  changeDir path = do
    _ <- lastDirByPath path
    return ()

  -- | Function 'listDir' lists provided absolute directory
  listDir :: FilePath -> Command FSMonad [FilePath]
  listDir path = do
    dir1 <- lastDirByPath path
    return $ Map.keys (getItems dir1)

  -- | Function 'getInfoOfFile' returns size and information about file from absolute 'FilePath'
  getInfoOfFile :: FilePath -> Command FSMonad (Integer, [String])
  getInfoOfFile path = do
    f <- fileByPath path
    let size = length $ (getData f)
    let perms = getFilePermissions f
    return (toInteger size, ["Path: " ++ path, "Permissions: " ++ show perms])

  -- | Function 'getInfoOfDir' returns information about dir from absolute 'FilePath'
  getInfoOfDir :: FilePath -> Command FSMonad [String]
  getInfoOfDir path = do
    d <- lastDirByPath path
    let perms = getDirectoryPermissions d
    return ["Path: " ++ path, "Permissions: " ++ show perms]

  -- | Function 'splitOnDirsAndFiles' splits 'FilePath' list from specific directory to files and dirs
  splitOnDirsAndFiles :: FilePath -> [FilePath] -> Command FSMonad ([FilePath], [FilePath])
  splitOnDirsAndFiles now pathList = implSplitOnDirsAndFilesFSMonad now pathList [] []
  readFileByPath :: FilePath -> Command FSMonad String
  readFileByPath path = do
    f <- fileByPath path
    return $ getData f

  -- | Function 'createFileFromPath' returns information about dir from absolute 'FilePath'
  createFileFromPath :: FilePath -> Command FSMonad ()
  createFileFromPath path = do
    let pathList = filePathToList path
    implMkdirOrFile True pathList
    return ()

  -- | Function 'createDirIfMissing' creates directory recursively by absolute 'FilePath'
  createDirIfMissing :: FilePath -> Command FSMonad ()
  createDirIfMissing path = do
    let pathList = filePathToList path
    implMkdirOrFile False pathList
    return ()

  -- | Function 'rmDirByPath' removes dir in provided absolute 'FilePath'
  rmDirByPath :: FilePath -> Command FSMonad ()
  rmDirByPath _ = do return ()

  -- | Function 'rmFileByPath' removes file in provided absolute 'FilePath'
  rmFileByPath :: FilePath -> Command FSMonad ()
  rmFileByPath _ = do return ()

  -- | Function 'writeStringToFile' writes string to absolute 'FilePath'
  writeStringToFile :: FilePath -> String -> Command FSMonad ()
  writeStringToFile _ _ = do return ()

  -- | Function 'copyFile' copies file from first provided absolute 'FilePath' to second
  copySourceFromFile :: FilePath -> FilePath -> Command FSMonad ()
  copySourceFromFile _ _ = do return ()

implSplitOnDirsAndFilesFSMonad :: FilePath -> [FilePath] -> [FilePath] -> [FilePath] -> Command FSMonad ([FilePath], [FilePath])
implSplitOnDirsAndFilesFSMonad _ [] files dirs = return (files, dirs)
implSplitOnDirsAndFilesFSMonad now (el : xs) files dirs = do
  nowNorm <- normalizeDirWithAbs el now
  fsElement <- lastElementByPath nowNorm
  case fsElement of
    ([], D d) -> implSplitOnDirsAndFilesFSMonad now xs files ((getDirectoryName d) : dirs)
    ([], F f) -> implSplitOnDirsAndFilesFSMonad now xs ((getFileName f) : files) dirs
    (_, _) -> throwError $ PathFormatError "invalid path"

-- | Function 'implNormalizePath' recursively normalizes path
implNormalizePath :: [FilePath] -> [FilePath] -> Command FSMonad [FilePath]
implNormalizePath [] result = return $ reverse result
implNormalizePath ("." : listTail) result = implNormalizePath listTail result
implNormalizePath (".." : _) ["$"] = throwError $ PathFormatError "trying to go up from root"
implNormalizePath (".." : listTail) (_ : result) = implNormalizePath listTail result
implNormalizePath (element : listTail) result = implNormalizePath listTail (element : result)

-- | Function 'checkRelative' checks if 'FilePath' is relative
checkRelative :: [FilePath] -> Bool
checkRelative ("$" : _) = False
checkRelative _ = True

-- | Function 'normalizePath' normalizes 'FilePath'
normalizePath :: [FilePath] -> Command FSMonad [FilePath]
normalizePath path = implNormalizePath path []

-- | Function 'listToFilepath' converts '[FilePath]' to one 'FilePath'
listToFilepath :: [FilePath] -> FilePath
listToFilepath pathList = joinWith '/' pathList

-- | Function 'filePathToList' converts 'FilePath' to one '[FilePath]'
filePathToList :: FilePath -> [FilePath]
filePathToList path = toList $ splitOn '/' path

-- | Function 'lastFileByPath' returns file on 'FilePath'
fileByPath :: FilePath -> Command FSMonad (File)
fileByPath path = do
  result <- lastElementByPath path
  case result of
    ([], F f) -> return f
    (_, _) -> throwError $ ChangingDirectoryError "no such file"

-- | Function 'lastDirByPath' returns last existing dir on 'FilePath'
lastDirByPath :: FilePath -> Command FSMonad Directory
lastDirByPath path = do
  result <- lastElementByPath path
  case result of
    ([], D d) -> return d
    (_, _) -> throwError $ ChangingDirectoryError "no such directory"

-- | Function 'lastElementByPath' returns element on 'FilePath'
lastElementByPath :: FilePath -> Command FSMonad ([FilePath], FSElement)
lastElementByPath path = do
  let pathList = filePathToList path
  rootDir <- lift $ lift $ (get :: FSMonad FileSystem)
  result <- lastElementByPathFromDir (getRoot rootDir) pathList
  return result
  where
    lastElementByPathFromDir :: Directory -> [FilePath] -> Command FSMonad ([FilePath], FSElement)
    lastElementByPathFromDir directory ("$" : fileTail) = implLastElementByPath directory fileTail
    lastElementByPathFromDir _ _ = throwError $ PathFormatError "it's not a directory"

-- | Function 'implLastElementByPath' recursively returns element on 'FilePath'
implLastElementByPath :: Directory -> [FilePath] -> Command FSMonad ([FilePath], FSElement)
implLastElementByPath directory [] = return ([], D directory)
implLastElementByPath directory (path : pathTail) =
  case Map.lookup path (getItems directory) of
    Nothing -> return ((path : pathTail), D directory)
    Just val -> case val of
      F f -> return (pathTail, F f)
      D d -> implLastElementByPath d pathTail

-- | Function 'implMkdirOrFile' recursively creates dir or file'
implMkdirOrFile :: Bool -> [FilePath] -> Command FSMonad ()
implMkdirOrFile flag ("$" : xs) = do
  fs <- lift $ lift $ (get :: FSMonad FileSystem)
  let dir1 = getRoot fs
  dir2 <- (mkdirOrFileImpl flag xs dir1)
  lift $ lift $ put (FileSystem dir2)
  return ()
  where
    mkdirOrFileImpl :: Bool -> [FilePath] -> Directory -> Command FSMonad Directory
    mkdirOrFileImpl True (fileName : []) (Directory name perms elementsMap) =
      case perms of
        False -> throwError $ PermissionsError "permission denied"
        True -> case Map.member fileName elementsMap of
                   True -> throwError $ CreatingFileError "name already exist"
                   False -> do
                     let dir1 = (Directory name perms (Map.insert fileName (F (File fileName True "")) elementsMap))
                     return dir1
    mkdirOrFileImpl True (nextDir : xs1) (Directory name perms elementsMap) =
      case perms of
        False -> throwError $ PermissionsError "permission denied"
        True -> do
          case Map.lookup nextDir elementsMap of
            Nothing -> throwError $ CreatingFileError "no such directory"
            Just e -> case e of
                        F _ -> throwError $ CreatingFileError ("not a directory" ++ show nextDir)
                        D d -> do
                          dir0 <- mkdirOrFileImpl True xs1 d
                          let dir1 = (Directory name perms (Map.insert nextDir (D dir0) elementsMap))
                          return dir1
    mkdirOrFileImpl True _ _ = throwError $ CreatingFileError "no such directory"
    mkdirOrFileImpl False (dirName : []) (Directory name perms elementsMap) =
      case perms of
        False -> throwError $ PermissionsError "permission denied"
        True ->
          case Map.member dirName elementsMap of
            True -> throwError $ CreatingDirectoryError "name already exist"
            False -> do
              let dir1 = (Directory name perms (Map.insert dirName (D (Directory dirName True (Map.empty))) elementsMap))
              return dir1
    mkdirOrFileImpl False (dirName : xs1) (Directory name perms elementsMap) =
      case perms of
        False -> throwError $ PermissionsError "permission denied"
        True ->
          case Map.lookup dirName elementsMap of
            Nothing -> do
                         dir0 <- mkdirOrFileImpl False xs1 (Directory dirName True Map.empty)
                         let dir1 = (Directory name perms (Map.insert dirName (D dir0) elementsMap))
                         return dir1
            Just e -> case e of
                        F _ -> throwError $ CreatingDirectoryError "not a directory"
                        D d -> do
                          dir0 <- mkdirOrFileImpl False xs1 d
                          let dir1 = (Directory name perms (Map.insert dirName (D dir0) elementsMap))
                          return dir1
    mkdirOrFileImpl False _ _ = throwError $ CreatingFileError "no such directory"
implMkdirOrFile _ _ = throwError $ PathFormatError "no valid absolute path"