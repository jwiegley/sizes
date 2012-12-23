{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Applicative
import           Control.Concurrent.ParallelIO
import           Control.DeepSeq
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.Function
import qualified Data.List as L
import           Data.Monoid
import           Data.Text as T hiding (filter, map, chunksOf)
import qualified Data.Text.Encoding as E
import           Debug.Trace
import           Filesystem (listDirectory, isFile)
import           Filesystem.Path.CurrentOS
import           GHC.Conc
import           Prelude hiding (FilePath, sequence, catch)
import           Stat
import           System.Console.CmdArgs
import           System.Environment (getArgs, withArgs)
import           System.Posix.Files
import           Text.Printf
import           Text.Regex.Posix
import           Unsafe.Coerce

default (Integer, Text)

version :: String
version = "2.0.4"

copyright :: String
copyright = "2012"

sizesSummary :: String
sizesSummary = "sizes v" ++ version ++ ", (C) John Wiegley " ++ copyright

data SizesOpts = SizesOpts { jobs         :: Int
                           , byCount      :: Bool
                           , annex        :: Bool
                           , apparent     :: Bool
                           , exclude      :: String
                           , minSize      :: Int
                           , minCount     :: Int
                           , blockSize    :: Int
                           , smalls       :: Bool
                           -- , dirsOnly  :: Bool
                           , depth        :: Int
                           , dirs         :: [String] }
               deriving (Data, Typeable, Show, Eq)

sizesOpts :: SizesOpts
sizesOpts = SizesOpts
    { jobs       = def &= name "j" &= typ "INT"
                       &= help "Run INT concurrent finds at once (default: 2)"
    , byCount    = def &= name "c" &= typ "BOOL"
                       &= help "Sort output by count (default: by size)"
    , annex      = def &= name "A" &= typ "BOOL"
                       &= help "Be mindful of how git-annex stores files"
    , apparent   = def &= typ "BOOL"
                       &= help "Print apparent sizes, rather than disk usage"
    , exclude    = def &= name "x" &= typ "REGEX"
                       &= help "Sort output by count (default: by size)"
    , minSize    = def &= name "m" &= typ "INT"
                       &= help "Smallest size to show, in MB (default: 10)"
    , minCount   = def &= name "M" &= typ "INT"
                       &= help "Smallest count to show (default: 100)"
    , blockSize  = def &= name "B" &= typ "INT"
                       &= help "Size of blocks on disk (default: 512)"
    , smalls     = def &= name "s" &= typ "BOOL"
                       &= help "Also show small (<1M && <100 files) entries"
    -- , dirsOnly = def &= typ "BOOL"
    --                  &= help "Show directories only"
    , depth      = def &= typ "INT"
                       &= help "Report entries to a depth of INT (default: 0)"
    , dirs       = def &= args &= typ "DIRS..." } &=
    summary sizesSummary &=
    program "sizes" &=
    help "Calculate amount of disk used by the given directories"

data EntryInfo = EntryInfo { _entryPath       :: FilePath
                           , _entryCount      :: Int
                           , _entryAllocSize  :: Int
                           , _entryIsDir      :: Bool }
               deriving Show

makeLenses ''EntryInfo

newEntry :: FilePath -> Bool -> EntryInfo
newEntry p = EntryInfo p 0 0

instance Monoid EntryInfo where
  mempty = newEntry "" False
  x `mappend` y = seq x $ seq y $
                  entryCount      +~ y^.entryCount $
                  entryAllocSize  +~ y^.entryAllocSize $ x

instance NFData EntryInfo where
  rnf a = a `seq` ()

main :: IO ()
main = do
  mainArgs <- getArgs
  opts     <- withArgs (if L.null mainArgs then [] else mainArgs)
                       (cmdArgs sizesOpts)
  _        <- GHC.Conc.setNumCapabilities $ case jobs opts of 0 -> 2; x -> x
  runSizes opts

runSizes :: SizesOpts -> IO ()
runSizes opts = do
  let dirsOpt = dirs opts
      directories = if L.null dirsOpt then ["."] else dirsOpt
  reportSizes opts $ map (fromText . pack) directories
  stopGlobalPool

reportEntryP :: SizesOpts -> EntryInfo -> Bool
reportEntryP opts entry = smalls opts
                          || entry^.entryAllocSize >= minSize'
                          || entry^.entryCount >= minCount'
  where
    minSize'  = (if minSize opts == 0 then 10 else minSize opts) * 1024^2
    minCount' = if minCount opts == 0 then 100 else minCount opts

reportSizes :: SizesOpts -> [FilePath] -> IO ()
reportSizes opts xs = do
  entryInfos <- parallel $ map reportSizesForDir xs
  let infos  = map fst entryInfos ++ L.concatMap snd entryInfos
      sorted = L.sortBy ((compare `on`) $
                         if byCount opts
                         then (^. entryCount)
                         else (^. entryAllocSize)) infos
  mapM_ reportEntry (filter (reportEntryP opts) sorted)

  where
    reportSizesForDir dir = do
      -- fsStatus <- getFilesystemStatus (E.encodeUtf8 (toTextIgnore dir))
      let fsBlkSize = statBlockSize -- filesystemBlockSize fsStatus
          opts'     = if blockSize opts == 0
                      then opts { blockSize = fromIntegral fsBlkSize }
                      else opts
      gatherSizesW opts' 0 dir

humanReadable :: Int -> String
humanReadable x
  | x < 1024   = printf "%db" x
  | x < 1024^2 = printf "%.0fK" (fromIntegral x / (1024 :: Double))
  | x < 1024^3 = printf "%.1fM" (fromIntegral x / (1024^2 :: Double))
  | x < 1024^4 = printf "%.2fG" (fromIntegral x / (1024^3 :: Double))
  | x < 1024^5 = printf "%.3fT" (fromIntegral x / (1024^4 :: Double))
  | x < 1024^6 = printf "%.3fP" (fromIntegral x / (1024^5 :: Double))
  | x < 1024^7 = printf "%.3fX" (fromIntegral x / (1024^6 :: Double))
  | otherwise  = printf "%db" x

reportEntry :: EntryInfo -> IO ()
reportEntry entry =
  let path = unpack (toTextIgnore (entry^.entryPath))
  in printf (unpack "%10s %10d  %s%s\n")
            (humanReadable (entry^.entryAllocSize))
            (entry^.entryCount) path
            (unpack $ if entry^.entryIsDir && L.last path /= '/'
                      then "/" else "")

toTextIgnore :: FilePath -> Text
toTextIgnore = either id id . toText

returnEmpty :: FilePath -> IO (EntryInfo, [EntryInfo])
returnEmpty path = return (newEntry path False, [])

gatherSizesW :: SizesOpts -> Int -> FilePath -> IO (EntryInfo, [EntryInfo])
gatherSizesW opts d p =
  catch (gatherSizes opts d p)
        (\e -> print (e :: IOException) >> returnEmpty p)

gatherSizes :: SizesOpts -> Int -> FilePath -> IO (EntryInfo, [EntryInfo])
gatherSizes opts curDepth path =
  gatherSizes' =<< if curDepth == 0
                   then getFileStatus path'
                   else getSymbolicLinkStatus path'
  where
    path'    = unpack (toTextIgnore path)
    annexRe  = unpack "\\.git/annex/"
    minSize' = (if minSize opts == 0 then 10 else minSize opts) * 1024^2

    gatherSizes' status
      | not (L.null (exclude opts)) && path' =~ exclude opts = returnEmpty path

      | isDirectory status = do
        files   <- listDirectory path
        entries <- (if curDepth == 0 then parallel else sequence) $
                   map (gatherSizesW opts (curDepth + 1) . collapse) files
        let firsts = map fst entries

        return ( L.foldl' mappend (newEntry path True) firsts
               , filter (reportEntryP opts) $
                 if curDepth <= depth opts
                 then firsts ++ L.concatMap snd entries
                 else [] )

      | (isRegularFile status && not (annex opts && path' =~ annexRe))
        || (annex opts && isSymbolicLink status) =
        catch (getFileSize status)
              (\e -> print (e :: IOException) >> returnEmpty path)

      | otherwise = returnEmpty path

    -- If status is for a symbolic link, it must be a Git-annex'd file
    getFileSize status = do
      status' <-
        if isSymbolicLink status
        then do
          destPath <- readSymbolicLink path'
          if destPath =~ annexRe
            then do
              let destFilePath  = fromText (T.pack destPath)
                  destPath'     = if relative destFilePath
                                  then T.unpack . toTextIgnore $
                                       parent path </> destFilePath
                                  else destPath
                  destFilePath' = fromText (T.pack destPath')
              exists <- isFile destFilePath'
              if exists
                then getFileStatus destPath'
                else return status
            else return status
        else return status

      let fsize     = fromIntegral $ fileSize status'
          blksize   = fromIntegral $ fileBlockSize (unsafeCoerce status')
          allocSize = if apparent opts
                      then fsize
                      else blksize * blockSize opts

      return (EntryInfo { _entryPath       = path
                        , _entryCount      = 1
                        , _entryAllocSize  = allocSize
                        , _entryIsDir      = False }, [])

-- Main.hs (sizes) ends here
