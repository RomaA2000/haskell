{-# LANGUAGE InstanceSigs #-}
{-
Module : Utils
Functions for manipulating with paths ans FSError type
-}
module Utils
  ( -- * The 'FSError' type
    FSError (..),

    -- * The 'splitOn' function
    splitOn,

    -- * The 'joinWith' function
    joinWith,

    -- * The 'getStringFromFSError' function
    getStringFromFSError,
  )
where

import Data.List.NonEmpty (NonEmpty ((:|)), fromList)

-- | Data 'FSError' represents File System Errors
data FSError
  = ChangingDirectoryError String
  | ListingDirectoryError String
  | NormalizingPathError String
  | CreatingDirectoryError String
  | CreatingFileError String
  | PermissionsError String
  | DirectoryNotFoundError String
  | PathFormatError String
  | WritingToFileError String
  | CopyingFileError String
  | RemovingFileError String
  | RemovingDirError String
  | ReadingFileError String
  | ReadingDirError String
  deriving (Eq, Show)

-- | Function 'splitOn' splits list with separator
splitOn :: Eq s => s -> [s] -> NonEmpty [s]
splitOn symbol = foldr splitOnImpl (fromList [[]])
  where
    splitOnImpl cur (listHead :| tailNotEmpty)
      | cur == symbol = [] :| (listHead : tailNotEmpty)
      | otherwise = (cur : listHead) :| tailNotEmpty

-- | Function 'joinWith' joins list with separator
joinWith :: s -> [[s]] -> [s]
joinWith symbol list = init $ foldr (joinWithImpl) [] list
  where
    joinWithImpl now chunk = now ++ symbol : chunk

-- | Function 'getStringFromFSError' returns string from 'FSError'
getStringFromFSError :: FSError -> String
getStringFromFSError (ChangingDirectoryError str) = str
getStringFromFSError (ListingDirectoryError str) = str
getStringFromFSError (NormalizingPathError str) = str
getStringFromFSError (CreatingDirectoryError str) = str
getStringFromFSError (CreatingFileError str) = str
getStringFromFSError (PermissionsError str) = str
getStringFromFSError (DirectoryNotFoundError str) = str
getStringFromFSError (PathFormatError str) = str
getStringFromFSError (WritingToFileError str) = str
getStringFromFSError (CopyingFileError str) = str
getStringFromFSError (RemovingFileError str) = str
getStringFromFSError (RemovingDirError str) = str
getStringFromFSError (ReadingFileError str) = str
getStringFromFSError (ReadingDirError str) = str
