{-# LANGUAGE ForeignFunctionInterface #-}

module Stat where

import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
import System.Posix.ByteString.FilePath
import System.Posix.Internals ( CFilePath )
import System.Posix.Types
import Unsafe.Coerce

#include "HsStat.h"

type CStat = ()
newtype FileStatus = FileStatus (ForeignPtr CStat)

statBlockSize :: FileOffset
statBlockSize = (#const S_BLKSIZE)

fileBlockSize :: FileStatus -> FileOffset
fileBlockSize (Stat.FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ (#peek struct stat, st_blocks)

type CStatvfs = ()
newtype FilesystemStatus = FilesystemStatus (ForeignPtr CStatvfs)

-- jww (2012-10-18): This will
foreign import ccall unsafe "HsBase.h __hscore_statvfs"
   c_statvfs :: CFilePath -> Ptr CStatvfs -> IO CInt

getFilesystemStatus :: RawFilePath -> IO Stat.FilesystemStatus
getFilesystemStatus path = do
  fp <- mallocForeignPtrBytes (#const sizeof(struct statvfs))
  withForeignPtr fp $ \p ->
    withFilePath path $ \s ->
      throwErrnoPathIfMinus1_ "getFilesystemStatus" path (c_statvfs s p)
  return (FilesystemStatus fp)

filesystemBlockSize :: FilesystemStatus -> CULong
filesystemBlockSize (Stat.FilesystemStatus statvfs) =
  unsafePerformIO $ withForeignPtr statvfs $ (#peek struct statvfs, f_frsize)

-- Stat.hsc ends here
