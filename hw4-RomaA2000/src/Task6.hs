{-# LANGUAGE RankNTypes #-}
{-
Module Task6:
Classes for file system.
-}
module Task6
  (
    -- | FileSystem type.
    FS(..),
    -- | getElement function.
    getElement,
    -- | contents function.
    contents,
    -- | name function.
    name,
    -- | _file function.
    _file,
    -- | _dir function.
    _dir,
    -- | _contents function.
    _contents
  )
where

import Lens.Micro
import System.Directory (doesDirectoryExist, listDirectory)

-- | FileSystem type.
data FS
  -- | Dir type.
  = Dir
  { -- | Name of directory
    name'     :: FilePath
   -- | Content of directory
  , contents' :: [FS]
  }
  -- | File type.
  | File
  { -- | Name of file
    name'     :: FilePath
  } deriving (Eq, Show)

-- | Copies FS from user's input.
getElement :: FilePath -> IO FS
getElement path = do
  flag <- doesDirectoryExist path
  case flag of
    False -> return $ File path
    True -> getDirectory path
  where
    getDirectory :: FilePath -> IO FS
    getDirectory p = do
      ls <- scan p
      return $ Dir p ls

    scan :: FilePath -> IO [FS]
    scan p = do
      ls <- listDirectory p
      mapM getElement ls

-- | Returns 'Lens'' from FS object into content' field.
contents :: Lens' FS [FS]
contents = lens contents' $ \v next ->
  case v of
    (Dir _ _) -> v {contents' = next}
    file      -> file

-- | Returns 'Traversal'' from FS object into itself if it is file.
_file :: Traversal' FS FS
_file f file@(File _) = f file
_file _ d = pure d

-- | Returns 'Traversal'' from FS object into itself if it is directory.
_dir :: Traversal' FS FS
_dir f d@(Dir _ _) = f d
_dir _ f = pure f

-- | Returns 'Lens'' from FS object into name' field.
name :: Lens' FS FilePath
name = lens name' $ \v next -> v {name' = next}

-- | Returns 'Traversal'' from FS object into content.
_contents :: Traversal' FS [FS]
_contents f (Dir n c) = Dir n <$> f c
_contents _ f = pure f
