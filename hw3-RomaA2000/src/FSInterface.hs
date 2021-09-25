{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-
Module FSInterface:
Interface for File System
-}
module FSInterface
  ( -- * The 'FSInterface' class
    FSInterface (..),

    -- * The 'Command' type
    Command,
  )
where

import Control.Monad.Except (ExceptT)
import Control.Monad.State (StateT, get, put)
import Utils (FSError(..))
import System.Directory
  ( canonicalizePath,
    copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getAccessTime,
    getFileSize,
    getPermissions,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
  )
import System.FilePath (isRelative, pathSeparator, takeExtension)
import Control.Monad.Except (throwError, lift)
import System.IO.Error (tryIOError, ioeGetErrorString)

-- | Type 'Command' represents File System and User
type Command m a = FSError (StateT FilePath m) a

-- | Class 'FSInterface' creates File System interface
class Monad m => FSInterface m where
  -- | Function 'normalizeDir' returns absolute 'FilePath'
  normalizeDir :: FilePath -> Command m FilePath
  normalizeDir path = do
    now <- get
    result <- normalizeDirWithAbs path now
    return result

  -- | Function 'cd' changes directory to provided
  cd :: FilePath -> Command m ()
  cd path = do
    normalizedPathTo <- normalizeDir path
    changeDir normalizedPathTo
    put normalizedPathTo
    return ()

  -- | Function 'ls' lists provided directory
  ls :: FilePath -> Command m [FilePath]
  ls path = do
    normalizedPathTo <- normalizeDir path
    result <- listDir normalizedPathTo
    return result

  -- | Function 'ls' lists directory from 'State' monad
  dir :: Command m [FilePath]
  dir = do
    now <- get
    result <- ls now
    return result

  -- | Function 'mkdir' creates directory recursively
  mkdir :: FilePath -> Command m ()
  mkdir path = do
    normalizedPathTo <- normalizeDir path
    createDirIfMissing normalizedPathTo
    return ()

  -- | Function 'writeToFile' writes 'String' to provided file
  writeToFile :: FilePath -> String -> Command m ()
  writeToFile path string = do
    normalizedPathTo <- normalizeDir path
    writeStringToFile normalizedPathTo string
    return ()

  -- | Function 'createFile' creates file in provided 'FilePath'
  createFile :: FilePath -> Command m ()
  createFile path = do
    normalizedPathTo <- normalizeDir path
    createFileFromPath normalizedPathTo
    return ()

  -- | Function 'rmFile' removes file in provided 'FilePath'
  rmFile :: FilePath -> Command m ()
  rmFile path = do
    normalizedPathTo <- normalizeDir path
    rmFileByPath normalizedPathTo
    return ()

  -- | Function 'copyFile' copies file from first provided 'FilePath' to second
  copyFile :: FilePath -> FilePath -> Command m ()
  copyFile pathTo pathFrom  = do
    normalizedPathTo <- normalizeDir pathTo
    normalizedPathFrom <- normalizeDir pathFrom
    copySourceFromFile normalizedPathTo normalizedPathFrom
    return ()

  -- | Function 'rmDir' removes dir in provided 'FilePath'
  rmDir :: FilePath -> Command m ()
  rmDir path = do
    normalizedPathTo <- normalizeDir path
    rmDirByPath normalizedPathTo
    return ()

  -- | Function 'cat' returns data from provided file
  cat :: FilePath -> Command m String
  cat path = do
    normalizedPathTo <- normalizeDir path
    result <- readFileByPath normalizedPathTo
    return result

  -- | Function 'findFile' finds in provided path specific filename
  findFile :: FilePath -> String -> Command m (Maybe FilePath)
  findFile path name = do
    now <- normalizeDir path
    allElem <- ls now
    result <- implFindFile now allElem name
    return result
    where
      implFindFile now allElem name1 = do
        (files, dirs) <- splitOnDirsAndFiles now allElem
        case filter (\f -> f == name1) files of
          (x : _) -> do
            newPath <- normalizeDirWithAbs x now
            return (Just newPath)
          [] -> dfsFileSearch dirs
            where
              dfsFileSearch [] = return Nothing
              dfsFileSearch (x : xs) = do
                directory <- normalizeDirWithAbs x now
                allElements <- ls directory
                ans <- implFindFile directory allElements name1
                case ans of
                  Nothing -> dfsFileSearch xs
                  Just val -> return $ Just val


  -- | Function 'getFileInfoFromPath' returns size and information about file
  getFileInfoFromPath :: FilePath -> Command m (Integer, [String])
  getFileInfoFromPath path = do
    normalizedPathTo <- normalizeDir path
    result <- getInfoOfFile normalizedPathTo
    return result

  -- | Function 'getDirInfoFromPath' returns number of inside files recursive size and information about directory
  getDirInfoFromPath :: FilePath -> Command m (Integer, Integer, [String])
  getDirInfoFromPath path = do
    normalizedPathTo <- normalizeDir path
    allElem <- ls normalizedPathTo
    str <- getInfoOfDir normalizedPathTo
    (numFiles, allSize) <- implDirInfoFromPath normalizedPathTo allElem
    return (numFiles, allSize, str)
    where
      implDirInfoFromPath now allElem = do
        (files, dirs) <- splitOnDirsAndFiles now allElem
        (num, allSize) <- getAllFiles now files (0, 0)
        (num1, allSize1) <- forEveryDir now dirs (0, 0)
        return (num + num1, allSize + allSize1)

      forEveryDir _ [] r = return r
      forEveryDir now (x : xs) (a, b) = do
        directory <- normalizeDirWithAbs x now
        allElem <- ls directory
        (num, sz) <- implDirInfoFromPath directory allElem
        forEveryDir now xs (num + a, sz + b)

      getAllFiles _ [] r = return r
      getAllFiles now (x : xs) (s, allS) = do
        path1 <- normalizeDirWithAbs x now
        (sz, _) <- getInfoOfFile path1
        getAllFiles now xs (s + 1, allS + sz)


  -- | Function 'normalizeDirWithAbs' returns absolute path from two provided
  normalizeDirWithAbs :: FilePath -> FilePath -> Command m FilePath

  -- | Function 'splitOnDirsAndFiles' splits 'FilePath' list from specific directory to files and dirs
  splitOnDirsAndFiles :: FilePath -> [FilePath] -> Command m ([FilePath], [FilePath])

  -- | Function 'readFileByPath' return string from absolute path to file
  readFileByPath :: FilePath -> Command m String

  -- | Function 'rmDirByPath' removes dir in provided absolute 'FilePath'
  rmDirByPath :: FilePath -> Command m ()

  -- | Function 'rmFileByPath' removes file in provided absolute 'FilePath'
  rmFileByPath :: FilePath -> Command m ()

  -- | Function 'copyFile' copies file from first provided absolute 'FilePath' to second
  copySourceFromFile :: FilePath -> FilePath -> Command m ()

  -- | Function 'getInfoOfFile' returns size and information about file from absolute 'FilePath'
  getInfoOfFile :: FilePath -> Command m (Integer, [String])

  -- | Function 'getInfoOfDir' returns information about dir from absolute 'FilePath'
  getInfoOfDir :: FilePath -> Command m [String]

  -- | Function 'createFileFromPath' returns information about dir from absolute 'FilePath'
  createFileFromPath :: FilePath -> Command m ()

  -- | Function 'changeDir' changes directory to provided absolute
  changeDir :: FilePath -> Command m ()

  -- | Function 'listDir' lists provided absolute directory
  listDir :: FilePath -> Command m [FilePath]

  -- | Function 'createDirIfMissing' creates directory recursively by absolute 'FilePath'
  createDirIfMissing :: FilePath -> Command m ()

  -- | Function 'writeStringToFile' writes string to absolute 'FilePath'
  writeStringToFile :: FilePath -> String -> Command m ()

-- | Instance 'FSInterface' creates File System for IO monad
instance FSInterface IO where
  -- | Function 'normalizeDirWithAbs' returns absolute path from two provided
  normalizeDirWithAbs :: FilePath -> FilePath -> Command IO FilePath
  normalizeDirWithAbs path now = do
    let newPath = if isRelative path then (now ++ [pathSeparator] ++ path) else path
    normalizedNewPath <- lift $ lift $ tryIOError $ canonicalizePath newPath
    case normalizedNewPath of
          Right val -> return val
          Left ex -> throwError $ NormalizingPathError $ ioeGetErrorString ex

  -- | Function 'changeDir' changes directory to provided absolute
  changeDir :: FilePath -> Command IO ()
  changeDir path = do
    result <- lift $ lift $ doesDirectoryExist path
    case result of
      True -> return ()
      False -> throwError $ ChangingDirectoryError $ "directory does not exist"

  -- | Function 'listDir' lists provided absolute directory
  listDir :: FilePath -> Command IO [FilePath]
  listDir path = do
    result <- lift $ lift $ tryIOError $ listDirectory path
    case result of
      Right val -> return val
      Left ex -> throwError $ ListingDirectoryError $ ioeGetErrorString ex

  -- | Function 'createDirIfMissing' creates directory recursively by absolute 'FilePath'
  createDirIfMissing :: FilePath -> Command IO ()
  createDirIfMissing path = do
    result <- lift $ lift $ tryIOError $ createDirectoryIfMissing True path
    case result of
      Right _ -> return ()
      Left ex -> throwError $ CreatingDirectoryError $ ioeGetErrorString ex

  -- | Function 'writeStringToFile' writes string to absolute 'FilePath'
  writeStringToFile :: FilePath -> String -> Command IO ()
  writeStringToFile path string = do
    result <- lift $ lift $ doesFileExist path
    case result of
      True -> createAndWriteFile path string
      False -> throwError $ WritingToFileError $ "file does not exist"

  -- | Function 'createFileFromPath' returns information about dir from absolute 'FilePath'
  createFileFromPath :: FilePath -> Command IO ()
  createFileFromPath path = createAndWriteFile path ""

  -- | Function 'getInfoOfFile' returns size and information about file from absolute 'FilePath'
  getInfoOfFile :: FilePath -> Command IO (Integer, [String])
  getInfoOfFile path = do
    result <- lift $ lift $ tryIOError $ System.Directory.getFileSize path
    case result of
      Left ex -> throwError $ ReadingFileError $ ioeGetErrorString ex
      Right size -> do
        info1 <- lift $ lift $ tryIOError $ getAccessTime path
        case info1 of
          Left ex -> throwError $ ReadingFileError $ ioeGetErrorString ex
          Right utcTime -> do
            info2 <- lift $ lift $ tryIOError $ getPermissions path
            case info2 of
              Left ex -> throwError $ ReadingFileError $ ioeGetErrorString ex
              Right permissions -> do
                let ext = takeExtension path
                let str = [ "Path: " ++ path,
                            "Permissions: " ++ show permissions,
                            "Access time: " ++ show utcTime,
                            "Extension: " ++ ext
                          ]
                return (size, str)

  -- | Function 'getInfoOfDir' returns information about dir from absolute 'FilePath'
  getInfoOfDir :: FilePath -> Command IO [String]
  getInfoOfDir path = do
    info <- lift $ lift $ tryIOError $ getPermissions path
    case info of
      Left ex -> throwError $ ReadingDirError $ ioeGetErrorString ex
      Right permissions -> return ["Path: " ++ path, "Permissions: " ++ show permissions]

  -- | Function 'copyFile' copies file from first provided absolute 'FilePath' to second
  copySourceFromFile :: FilePath -> FilePath -> Command IO ()
  copySourceFromFile pathFrom pathTo = do
    result <- lift $ lift $ tryIOError $ System.Directory.copyFile pathFrom pathTo
    case result of
      Right val -> return val
      Left ex -> throwError $ CopyingFileError $ ioeGetErrorString ex

  -- | Function 'rmFileByPath' removes file in provided absolute 'FilePath'
  rmFileByPath :: FilePath -> Command IO ()
  rmFileByPath path = do
    result <- lift $ lift $ tryIOError $ removeFile path
    case result of
      Right _ -> return ()
      Left ex -> throwError $ RemovingFileError $ ioeGetErrorString ex

  -- | Function 'rmDirByPath' removes dir in provided absolute 'FilePath'
  rmDirByPath :: FilePath -> Command IO ()
  rmDirByPath path = do
    result <- lift $ lift $ tryIOError $ removeDirectoryRecursive path
    case result of
      Right _ -> return ()
      Left ex -> throwError $ RemovingDirError $ ioeGetErrorString ex

  -- | Function 'readFileByPath' return string from absolute path to file
  readFileByPath :: FilePath -> Command IO String
  readFileByPath path = do
    result <- lift $ lift $ tryIOError (Prelude.readFile path)
    case result of
      Right str -> return str
      Left ex -> throwError $ ReadingFileError $ ioeGetErrorString ex

  -- | Function 'splitOnDirsAndFiles' splits 'FilePath' list from specific directory to files and dirs
  splitOnDirsAndFiles :: FilePath -> [FilePath] -> Command IO ([FilePath], [FilePath])
  splitOnDirsAndFiles now pathList = implSplitOnDirsAndFilesIO now pathList [] []

-- | Function 'implSplitOnDirsAndFilesIO' splits 'FilePath' list fo files and dirs in IO monad
implSplitOnDirsAndFilesIO :: FilePath -> [FilePath] -> [FilePath] -> [FilePath] -> Command IO ([FilePath], [FilePath])
implSplitOnDirsAndFilesIO _ [] files dirs = return (files, dirs)
implSplitOnDirsAndFilesIO now (el : xs) files dirs = do
  nowNorm <- normalizeDirWithAbs el now
  boolFile <- lift $ lift $ doesFileExist nowNorm
  case boolFile of
    True -> implSplitOnDirsAndFilesIO now xs (el : files) dirs
    False -> do
      boolDir <- lift $ lift $ doesDirectoryExist nowNorm
      case boolDir of
        True -> implSplitOnDirsAndFilesIO now xs files (el : dirs)
        False -> implSplitOnDirsAndFilesIO now xs files dirs

-- | Function 'createAndWriteFile' creates file by absolute 'FilePath' and writes 'String' in it
createAndWriteFile :: FilePath -> String -> Command IO ()
createAndWriteFile path string = do
  result <- lift $ lift $ tryIOError (Prelude.writeFile path string)
  case result of
    Right _ -> return ()
    Left ex -> throwError $ WritingToFileError $ ioeGetErrorString ex
