{-# LINE 1 "Stat.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LINE 2 "Stat.hsc" #-}

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


{-# LINE 17 "Stat.hsc" #-}

type CStat = ()
newtype FileStatus = FileStatus (ForeignPtr CStat)

statBlockSize :: FileOffset
statBlockSize = (512)
{-# LINE 23 "Stat.hsc" #-}

fileBlockSize :: FileStatus -> FileOffset
fileBlockSize (Stat.FileStatus stat) =
  unsafePerformIO $ withForeignPtr stat $ ((\hsc_ptr -> peekByteOff hsc_ptr 104))
{-# LINE 27 "Stat.hsc" #-}

type CStatvfs = ()
newtype FilesystemStatus = FilesystemStatus (ForeignPtr CStatvfs)

-- jww (2012-10-18): This will
foreign import ccall unsafe "HsBase.h __hscore_statvfs"
   c_statvfs :: CFilePath -> Ptr CStatvfs -> IO CInt

getFilesystemStatus :: RawFilePath -> IO Stat.FilesystemStatus
getFilesystemStatus path = do
  fp <- mallocForeignPtrBytes (64)
{-# LINE 38 "Stat.hsc" #-}
  withForeignPtr fp $ \p ->
    withFilePath path $ \s ->
      throwErrnoPathIfMinus1_ "getFilesystemStatus" path (c_statvfs s p)
  return (FilesystemStatus fp)

filesystemBlockSize :: FilesystemStatus -> CULong
filesystemBlockSize (Stat.FilesystemStatus statvfs) =
  unsafePerformIO $ withForeignPtr statvfs $ ((\hsc_ptr -> peekByteOff hsc_ptr 8))
{-# LINE 46 "Stat.hsc" #-}

-- Stat.hsc ends here
