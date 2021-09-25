{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-
Module Main:
Command line tool with File System Interface
-}
module Main
  ( -- * The 'main' function
    main,
  )
where

import Control.Monad.Except (ExceptT, catchError, lift, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (execStateT, get, lift)
import System.Directory (doesDirectoryExist)
import Data.List.NonEmpty (NonEmpty ((:|)), fromList, toList)
import FSInterface (Command, FSInterface (..))
import Options.Applicative
  ( Parser (..),
    ParserInfo (..),
    command,
    defaultPrefs,
    execParserPure,
    fullDesc,
    getParseResult,
    helper,
    info,
    progDesc,
    strArgument,
    subparser,
  )
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Utils (FSError (..))

-- | Function 'exceptionHandler' creates error message from error and path
exceptionHandler :: FilePath -> FSError -> Command IO ()
exceptionHandler path exception = do
  case exception of
    ListingDirectoryError str -> lift $ lift $ putStrLn $ ("Error listing directory " ++ path ++ " " ++ str)
    ChangingDirectoryError str -> lift $ lift $ putStrLn $ ("Error changing directory to " ++ path ++ " " ++ str)
    NormalizingPathError str -> lift $ lift $ putStrLn $ ("Error normalizing " ++ path ++ ", " ++ str)
    CreatingDirectoryError str -> lift $ lift $ putStrLn $ ("Error creating directory " ++ path ++ ", " ++ str)
    PermissionsError str -> lift $ lift $ putStrLn $ ("Error with permission " ++ path ++ ", " ++ str)
    DirectoryNotFoundError str -> lift $ lift $ putStrLn $ ("Error finding directory " ++ path ++ ", " ++ str)
    PathFormatError str -> lift $ lift $ putStrLn $ ("Error with path format " ++ path ++ ", " ++ str)
    WritingToFileError str -> lift $ lift $ putStrLn $ ("Error writing to file " ++ path ++ ", " ++ str)
    CopyingFileError str -> lift $ lift $ putStrLn $ ("Error copying files " ++ path ++ ", " ++ str)
    RemovingFileError str -> lift $ lift $ putStrLn $ ("Error removing file " ++ path ++ ", " ++ str)
    RemovingDirError str -> lift $ lift $ putStrLn $ ("Error removing dir " ++ path ++ ", " ++ str)
    ReadingFileError str -> lift $ lift $ putStrLn $ ("Error reading file " ++ path ++ ", " ++ str)

-- | Data 'Queries' represents commands
data Queries
  = Help
  | CD FilePath
  | RMF FilePath
  | RMD FilePath
  | MKDIR FilePath
  | CAT FilePath
  | LS FilePath
  | TOUCH FilePath
  | FILEINFO FilePath
  | WRITE FilePath String
  | COPY FilePath FilePath
  | FIND FilePath String
  | DIRINFO FilePath
  | DIR
  | EXIT
  | HELP
  deriving (Show)

-- | Function 'getHelpStr' returns help 'String'
getHelpStr :: [String]
getHelpStr =
  [ "<folder> -- some folder",
    "<filename> -- some file",
    "<filepath> -- some <folder> concatenated with <filename> with path separator",
    "cd <folder> -- change directory",
    "ls <folder> -- list another directory",
    "dir -- list this directory",
    "mkdir <folder> -- recursively creates directories",
    "touch <filepath> -- creates file",
    "write-file <filepath> <string> -- writes string to file",
    "copy <filepath> <filepath> -- copies file to another path",
    "cat <filepath> -- prints file data",
    "rmf <filepath> -- removes file",
    "rmd <folder> -- removes directory recursively",
    "findf <folder> <filename> -- finds file in folder recursively",
    "infof <filepath> -- prints file information",
    "infod <folder> -- prints directory information",
    "exit -- exits command line",
    "help -- you know what it does"
  ]

-- | Function 'parserSetting' creates 'ParserInfo Queries' instance
parserSetting :: ParserInfo Queries
parserSetting = information parsing "This is the main prog desc"
  where
    parsing :: Parser Queries
    parsing =
      (subparser . foldMap cm)
        [ ("cd", cdP),
          ("ls", lsP),
          ("dir", pure DIR),
          ("mkdir", mkdirP),
          ("touch", touchP),
          ("write-file", writeP),
          ("copy", copyP),
          ("cat", catP),
          ("rmf", rmfP),
          ("rmd", rmdP),
          ("findf", findP),
          ("infof", infoFP),
          ("infod", infoDP),
          ("help", pure HELP),
          ("exit", pure EXIT)
        ]
    destFolderArg =
      strArgument
        (mconcat [])

    information :: Parser a -> String -> ParserInfo a
    information p desc = info (helper <*> p) (fullDesc <> progDesc desc)

    cm (cmdName, parser) = command cmdName (information parser "")

    cdP = CD <$> destFolderArg
    lsP = LS <$> destFolderArg
    mkdirP = MKDIR <$> destFolderArg
    touchP = TOUCH <$> destFolderArg
    writeP = WRITE <$> destFolderArg <*> destFolderArg
    copyP = COPY <$> destFolderArg <*> destFolderArg
    catP = CAT <$> destFolderArg
    rmfP = RMF <$> destFolderArg
    rmdP = RMD <$> destFolderArg
    findP = FIND <$> destFolderArg <*> destFolderArg
    infoFP = FILEINFO <$> destFolderArg
    infoDP = DIRINFO <$> destFolderArg

-- | Function 'commandLoop' reads from cmd
commandLoop :: Command IO ()
commandLoop = do
  x <- get
  liftIO . putStr $ x ++ "> "
  cmd <- liftIO getLine
  let val = execParserPure defaultPrefs parserSetting $ words cmd
  case (getParseResult val) of
    Nothing -> do
      liftIO . putStrLn $ "command not found, try help for list of commands"
      commandLoop
    Just result -> do
      case result of
        CD path -> do
          (cd path) `catchError` (\ex -> exceptionHandler path ex)
          commandLoop
        LS path -> do
          do
            r <- ls path
            mapM_ (liftIO . putStrLn) r
            `catchError` (\ex -> exceptionHandler path ex)
          commandLoop
        DIR -> do
          do
            r <- dir
            mapM_ (liftIO . putStrLn) r
            `catchError` (\ex -> exceptionHandler "." ex)
          commandLoop
        MKDIR path -> do
          mkdir path `catchError` (\ex -> exceptionHandler path ex)
          commandLoop
        TOUCH path -> do
          createFile path `catchError` (\ex -> exceptionHandler path ex)
          commandLoop
        WRITE path str -> do
          (writeToFile path str) `catchError` (\ex -> exceptionHandler path ex)
          commandLoop
        COPY path1 path2 -> do
          (copyFile path1 path2) `catchError` (\ex -> exceptionHandler path1 ex)
          commandLoop
        CAT path -> do
          do
            r <- cat path
            (liftIO . putStrLn) r
            `catchError` (\ex -> exceptionHandler "." ex)
          commandLoop
        RMF path -> do
          rmFile path `catchError` (\ex -> exceptionHandler path ex)
          commandLoop
        RMD path -> do
          rmDir path `catchError` (\ex -> exceptionHandler path ex)
          commandLoop
        FIND path name -> do
          do
            r <- findFile path name
            case r of
              Nothing -> (liftIO . putStrLn) "file not found"
              Just v -> (liftIO . putStrLn) v
            `catchError` (\ex -> exceptionHandler path ex)
          commandLoop
        FILEINFO path -> do
          do
            (size, str) <- getFileInfoFromPath path
            (liftIO . putStrLn) ("Size: " ++ show size)
            mapM_ (liftIO . putStrLn) str
            `catchError` (\ex -> exceptionHandler path ex)
          commandLoop
        DIRINFO path -> do
          do
            (num, size, str) <- getDirInfoFromPath path
            (liftIO . putStrLn) ("Number of files: " ++ show num)
            (liftIO . putStrLn) ("Size: " ++ show size)
            mapM_ (liftIO . putStrLn) str
            `catchError` (\ex -> exceptionHandler path ex)
          commandLoop
        EXIT -> return ()
        HELP -> do
          mapM_ (liftIO . putStrLn) getHelpStr
          commandLoop

-- | Function 'preLoop' for state initing
preLoop :: IO ()
preLoop = do
  putStrLn "Enter starting directory: "
  path <- liftIO getLine
  flag <- doesDirectoryExist path
  if flag then do
    _ <- execStateT (runExceptT commandLoop) path
    return ()
  else do
    putStrLn "No such directory"
    preLoop
    return ()

-- | Function 'main' entry point
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  preLoop
  return ()
