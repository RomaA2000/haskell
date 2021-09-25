{-# LANGUAGE RankNTypes #-}
{-
Module Task7:
Functions for file system.
-}
module Task7
  ( -- * The 'ls' implementation
    ls,
    -- * The 'file' implementation
    file,
    -- * The 'cd' implementation
    cd,
    -- * The 'ls'' implementation
    ls',
    -- * The 'file'' implementation
    file'
  ) where

import Task6
import Lens.Micro

-- | 'Traversal'' that filterers FS '_contents'.
filt :: (FS -> Bool) -> Traversal' FS FS
filt p = _contents . each . filtered p

-- | Creates 'Traversal'' from a FS to file.
file' :: FilePath -> Traversal' FS FS
file' path = filt (\v -> (name' v == path) && (has _file v))

-- | 'Traversal'' to all FS content.
ls' :: Traversal' FS FilePath
ls' = filt (const True) . name

-- | Creates 'Traversal'' from a FS to subdirectory.
cd :: FilePath -> Traversal' FS FS
cd path = filt (\v -> (name' v == path) && (has _dir v))

-- | Returns all FS content.
ls :: FS -> [FilePath]
ls val = val ^.. ls'

-- | Returns filename from FS of Nothing.
file :: FS -> FilePath -> Maybe FilePath
file val path = val ^.. file' path . name ^? each
