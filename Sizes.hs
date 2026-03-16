{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Sizes (
    -- * Main entry point
    sizesMain,

    -- * Types
    SizesOpts (..),
    EntryInfo (..),
    SeenInodes,

    -- * Lenses
    entryPath,
    entryCount,
    entryAllocSize,
    entryIsDir,

    -- * Functions
    humanReadable,
    reportEntryP,
    newEntry,
) where

-- jww (2013-08-23): Still need to deal with hard-links.

import Control.Concurrent.ParallelIO
import Control.DeepSeq
import Control.Exception hiding (catch)
import Control.Lens
import Control.Monad
import Control.Monad.Catch (catch)
import Control.Monad.State.Strict
import Data.DList (DList)
import qualified Data.DList as DL
import Data.Function
import qualified Data.List as L
import qualified Data.Set as Set
import Data.Text as T hiding (chunksOf, filter, map)
import Filesystem (isFile, listDirectory)
import Filesystem.Path.CurrentOS
import GHC.Conc
import Stat
import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Posix.Files hiding (fileBlockSize)
import System.Posix.Types (DeviceID, FileID)
import Text.Printf
import Text.Regex.PCRE
import Unsafe.Coerce
import Prelude hiding (FilePath, sequence)

default (Integer, Text)

version :: String
version = "2.4.2"

copyright :: String
copyright = "2025"

sizesSummary :: String
sizesSummary = "sizes v" ++ version ++ ", (C) John Wiegley " ++ copyright

data SizesOpts = SizesOpts
    { jobs :: Int
    , byCount :: Bool
    , annex :: Bool
    , apparent :: Bool
    , baseTen :: Bool
    , exclude :: String
    , minSize :: Int
    , minCount :: Int
    , blockSize :: Int
    , smalls :: Bool
    , dedupeLinks :: Bool
    , -- , dirsOnly  :: Bool
      depth :: Int
    , dirs :: [String]
    }
    deriving (Data, Typeable, Show, Eq)

sizesOpts :: SizesOpts
sizesOpts =
    SizesOpts
        { jobs =
            def
                &= name "j"
                &= typ "INT"
                &= help "Run INT concurrent finds at once (default: 2)"
        , byCount =
            def
                &= name "c"
                &= typ "BOOL"
                &= help "Sort output by count (default: by size)"
        , annex =
            def
                &= name "A"
                &= typ "BOOL"
                &= help "Be mindful of how git-annex stores files"
        , apparent =
            def
                &= typ "BOOL"
                &= help "Print apparent sizes, rather than disk usage"
        , baseTen =
            def
                &= name "H"
                &= typ "BOOL"
                &= help "Print amounts divided by 1000 rather than 1024"
        , exclude =
            def
                &= name "x"
                &= typ "REGEX"
                &= help "Exclude files whose path matches the REGEX"
        , minSize =
            def
                &= name "m"
                &= typ "INT"
                &= help "Smallest size to show, in MB (default: 10)"
        , minCount =
            def
                &= name "M"
                &= typ "INT"
                &= help "Smallest count to show (default: 100)"
        , blockSize =
            def
                &= name "B"
                &= typ "INT"
                &= help "Size of blocks on disk (default: 512)"
        , smalls =
            def
                &= name "s"
                &= typ "BOOL"
                &= help "Also show small (<1M && <100 files) entries"
        , dedupeLinks =
            def
                &= name "L"
                &= typ "BOOL"
                &= help "Deduplicate hard links (count each inode only once)"
        , -- , dirsOnly = def &= typ "BOOL"
          --                  &= help "Show directories only"
          depth =
            def
                &= typ "INT"
                &= help "Report entries to a depth of INT (default: 1)"
        , dirs = def &= args &= typ "DIRS..."
        }
        &= summary sizesSummary
        &= program "sizes"
        &= help "Calculate amount of disk used by the given directories"

data EntryInfo = EntryInfo
    { _entryPath :: FilePath
    , _entryCount :: Int
    , _entryAllocSize :: Int
    , _entryIsDir :: Bool
    }
    deriving (Show, Eq)

makeLenses ''EntryInfo

-- Track (DeviceID, FileID) pairs to detect hard links
type SeenInodes = Set.Set (DeviceID, FileID)

newEntry :: FilePath -> Bool -> EntryInfo
newEntry p = EntryInfo p 0 0

instance Semigroup EntryInfo where
    x <> y =
        seq x $
            seq y $
                entryCount +~ y ^. entryCount $
                    entryAllocSize +~ y ^. entryAllocSize $
                        x

instance Monoid EntryInfo where
    mempty = newEntry "" False

instance NFData EntryInfo where
    rnf a = a `seq` ()

sizesMain :: IO ()
sizesMain = do
    mainArgs <- getArgs
    opts <-
        withArgs
            (if L.null mainArgs then [] else mainArgs)
            (cmdArgs sizesOpts)
    GHC.Conc.setNumCapabilities $ case jobs opts of 0 -> 2; x -> x
    runSizes $ case depth opts of 0 -> opts{depth = 1}; _ -> opts

runSizes :: SizesOpts -> IO ()
runSizes opts = do
    let dirsOpt = dirs opts
        directories = if L.null dirsOpt then ["."] else dirsOpt
    reportSizes opts $ Prelude.map (fromText . pack) directories
    stopGlobalPool

reportEntryP :: SizesOpts -> EntryInfo -> Bool
reportEntryP opts entry =
    smalls opts
        || entry ^. entryAllocSize >= minSize'
        || entry ^. entryCount >= minCount'
  where
    minSize' =
        (if minSize opts == 0 then 10 else minSize opts)
            * (if baseTen opts then 1000 else 1024) ^ (2 :: Int)
    minCount' = if minCount opts == 0 then 100 else minCount opts

reportSizes :: SizesOpts -> [FilePath] -> IO ()
reportSizes opts xs = do
    entryInfos <- parallel $ Prelude.map reportSizesForDir xs
    let infos =
            Prelude.map fst entryInfos
                ++ DL.toList (DL.concat (Prelude.map snd entryInfos))
        sorted =
            L.sortBy
                ( (compare `on`) $
                    if byCount opts
                        then (^. entryCount)
                        else (^. entryAllocSize)
                )
                infos
    mapM_
        (reportEntry (baseTen opts))
        (Prelude.filter (reportEntryP opts) sorted)
  where
    reportSizesForDir dir =
        -- fsStatus <- getFilesystemStatus (E.encodeUtf8 (toTextIgnore dir))
        let fsBlkSize = statBlockSize -- filesystemBlockSize fsStatus
            opts' =
                if blockSize opts == 0
                    then opts{blockSize = fromIntegral fsBlkSize}
                    else opts
         in fst <$> runStateT (gatherSizes opts' 0 dir) Set.empty

humanReadable :: Int -> Int -> String
humanReadable x d
    | x < d = printf "%db" x
    | x < d ^ (2 :: Int) = printf "%.0fK" (fromIntegral x / (fromIntegral d :: Double))
    | x < d ^ (3 :: Int) = printf "%.1fM" (fromIntegral x / (fromIntegral d ^ (2 :: Int) :: Double))
    | x < d ^ (4 :: Int) = printf "%.2fG" (fromIntegral x / (fromIntegral d ^ (3 :: Int) :: Double))
    | x < d ^ (5 :: Int) = printf "%.3fT" (fromIntegral x / (fromIntegral d ^ (4 :: Int) :: Double))
    | x < d ^ (6 :: Int) = printf "%.3fP" (fromIntegral x / (fromIntegral d ^ (5 :: Int) :: Double))
    | x < d ^ (7 :: Int) = printf "%.3fX" (fromIntegral x / (fromIntegral d ^ (6 :: Int) :: Double))
    | otherwise = printf "%db" x

reportEntry :: Bool -> EntryInfo -> IO ()
reportEntry bTen entry =
    let path = T.unpack (toTextIgnore (entry ^. entryPath))
     in printf
            (T.unpack "%10s %10d  %s%s\n")
            (humanReadable (entry ^. entryAllocSize) (if bTen then 1000 else 1024))
            (entry ^. entryCount)
            path
            ( T.unpack $
                if entry ^. entryIsDir && L.last path /= '/'
                    then "/"
                    else ""
            )

toTextIgnore :: FilePath -> Text
toTextIgnore = either id id . toText

returnEmpty :: FilePath -> StateT SeenInodes IO (EntryInfo, DList EntryInfo)
returnEmpty path = return (newEntry path False, DL.empty)

gatherSizes :: SizesOpts -> Int -> FilePath -> StateT SeenInodes IO (EntryInfo, DList EntryInfo)
gatherSizes opts curDepth path = do
    excl <-
        if L.null (exclude opts)
            then return $ Right False
            else liftIO $ try $ return $ path' =~ exclude opts -- jww (2013-08-15): poor
    case excl of
        Left (_ :: SomeException) -> returnEmpty path
        Right True -> returnEmpty path
        _ ->
            ( do
                status <-
                    liftIO
                        ( if curDepth == 0
                            then getFileStatus path'
                            else getSymbolicLinkStatus path'
                        )
                go status
            )
                `catch` ( \e -> do
                            liftIO $ putStrLn $ path' ++ ": " ++ Prelude.show (e :: IOException)
                            returnEmpty path
                        )
  where
    pathT = toTextIgnore path
    path' = T.unpack pathT

    go status
        | isDirectory status =
            foldM
                ( \(y, ys) x -> do
                    (x', xs') <- gatherSizes opts (curDepth + 1) (collapse x)
                    let x'' = y <> x'
                        xs'' =
                            if curDepth < depth opts
                                then ys <> DL.singleton x' <> xs'
                                else DL.empty
                    return $! x'' `seq` xs'' `seq` (x'', xs'')
                )
                (newEntry path True, DL.empty)
                =<< liftIO (listDirectory path)
        | ( isRegularFile status
                && not (annex opts && ".git/annex/" `isInfixOf` pathT)
          )
            || (annex opts && isSymbolicLink status) = do
            status' <-
                -- If status is for a symbolic link, it must be a Git-annex'd file
                if isSymbolicLink status
                    then do
                        destPath <- liftIO $ readSymbolicLink path'
                        if ".git/annex/" `L.isInfixOf` destPath
                            then do
                                let destFilePath = fromText (T.pack destPath)
                                    destPath' =
                                        if relative destFilePath
                                            then
                                                T.unpack . toTextIgnore $
                                                    parent path </> destFilePath
                                            else destPath
                                    destFilePath' = fromText (T.pack destPath')
                                exists <- liftIO $ isFile destFilePath'
                                if exists
                                    then liftIO $ getFileStatus destPath'
                                    else return status
                            else return status
                    else return status

            -- Check for hard link deduplication
            let dev = deviceID status'
                ino = fileID status'
                numLinks = linkCount status'
                shouldDedupe = dedupeLinks opts && numLinks > 1

            -- If deduplication is enabled and this has multiple hard links, check if we've seen it
            alreadySeen <-
                if shouldDedupe
                    then do
                        seen <- get
                        if Set.member (dev, ino) seen
                            then return True
                            else do
                                put $ Set.insert (dev, ino) seen
                                return False
                    else return False

            let fsize = fileSize status'
                blksize = fileBlockSize (unsafeCoerce status')
                allocSize
                    | alreadySeen = 0 -- Don't count size if we've already seen this inode
                    | apparent opts = fromIntegral fsize
                    | otherwise = fromIntegral blksize * blockSize opts

            return
                ( EntryInfo
                    { _entryPath = path
                    , _entryCount = if alreadySeen then 0 else 1
                    , _entryAllocSize = allocSize
                    , _entryIsDir = False
                    }
                , DL.empty
                )
        | otherwise = returnEmpty path

-- Sizes.hs ends here
