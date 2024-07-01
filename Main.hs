{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

-- jww (2013-08-23): Still need to deal with hard-links.

import           Control.Applicative
import           Control.Concurrent.ParallelIO
import           Control.DeepSeq
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.DList (DList)
import qualified Data.DList as DL
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
import           System.Posix.Files hiding (fileBlockSize)
import           Text.Printf
import           Text.Regex.PCRE
import           Unsafe.Coerce

default (Integer, Text)

version :: String
version = "2.4.0"

copyright :: String
copyright = "2024"

sizesSummary :: String
sizesSummary = "sizes v" ++ version ++ ", (C) John Wiegley " ++ copyright

data SizesOpts = SizesOpts { jobs         :: Int
                           , byCount      :: Bool
                           , annex        :: Bool
                           , apparent     :: Bool
                           , baseTen      :: Bool
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
    , baseTen    = def &= name "H" &= typ "BOOL"
                       &= help "Print amounts divided by 1000 rather than 1024"
    , exclude    = def &= name "x" &= typ "REGEX"
                       &= help "Exclude files whose path matches the REGEX"
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
                       &= help "Report entries to a depth of INT (default: 1)"
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

instance Semigroup EntryInfo where
  x <> y = seq x $ seq y $
           entryCount      +~ y^.entryCount $
           entryAllocSize  +~ y^.entryAllocSize $ x

instance Monoid EntryInfo where
  mempty = newEntry "" False
  mappend = (<>)

instance NFData EntryInfo where
  rnf a = a `seq` ()

main :: IO ()
main = do
  mainArgs <- getArgs
  opts     <- withArgs (if L.null mainArgs then [] else mainArgs)
                      (cmdArgs sizesOpts)
  _        <- GHC.Conc.setNumCapabilities $ case jobs opts of 0 -> 2; x -> x
  runSizes $ case depth opts of 0 -> opts { depth = 1 }; _ -> opts

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
    minSize'  = (if minSize opts == 0 then 10 else minSize opts) *
        (if baseTen opts then 1000 else 1024)^2
    minCount' = if minCount opts == 0 then 100 else minCount opts

reportSizes :: SizesOpts -> [FilePath] -> IO ()
reportSizes opts xs = do
  entryInfos <- parallel $ map reportSizesForDir xs
  let infos  = map fst entryInfos ++
               DL.toList (DL.concat (map snd entryInfos))
      sorted = L.sortBy ((compare `on`) $
                         if byCount opts
                         then (^. entryCount)
                         else (^. entryAllocSize)) infos
  mapM_ (reportEntry (baseTen opts)) (filter (reportEntryP opts) sorted)

  where
    reportSizesForDir =
      -- fsStatus <- getFilesystemStatus (E.encodeUtf8 (toTextIgnore dir))
      let fsBlkSize = statBlockSize -- filesystemBlockSize fsStatus
          opts'     = if blockSize opts == 0
                      then opts { blockSize = fromIntegral fsBlkSize }
                      else opts
      in gatherSizes opts' 0

humanReadable :: Int -> Int -> String
humanReadable x div
  | x < div   = printf "%db" x
  | x < div^2 = printf "%.0fK" (fromIntegral x / (fromIntegral div :: Double))
  | x < div^3 = printf "%.1fM" (fromIntegral x / (fromIntegral div^2 :: Double))
  | x < div^4 = printf "%.2fG" (fromIntegral x / (fromIntegral div^3 :: Double))
  | x < div^5 = printf "%.3fT" (fromIntegral x / (fromIntegral div^4 :: Double))
  | x < div^6 = printf "%.3fP" (fromIntegral x / (fromIntegral div^5 :: Double))
  | x < div^7 = printf "%.3fX" (fromIntegral x / (fromIntegral div^6 :: Double))
  | otherwise  = printf "%db" x

reportEntry :: Bool -> EntryInfo -> IO ()
reportEntry bTen entry =
  let path = unpack (toTextIgnore (entry^.entryPath))
  in printf
     (unpack "%10s %10d  %s%s\n")
     (humanReadable (entry^.entryAllocSize) (if bTen then 1000 else 1024))
     (entry^.entryCount) path
     (unpack $ if entry^.entryIsDir && L.last path /= '/'
               then "/" else "")

toTextIgnore :: FilePath -> Text
toTextIgnore = either id id . toText

returnEmpty :: FilePath -> IO (EntryInfo, DList EntryInfo)
returnEmpty path = return (newEntry path False, DL.empty)

gatherSizes :: SizesOpts -> Int -> FilePath -> IO (EntryInfo, DList EntryInfo)
gatherSizes opts curDepth path = do
  excl <- if L.null (exclude opts)
          then return $ Right False
          else try $ return $ path' =~ exclude opts -- jww (2013-08-15): poor
  case excl of
    Left (_ :: SomeException) -> returnEmpty path
    Right True -> returnEmpty path
    _ ->
      catch (go =<< if curDepth == 0
                    then getFileStatus path'
                    else getSymbolicLinkStatus path')
            (\e -> do putStrLn $ path' ++ ": " ++ show (e :: IOException)
                      returnEmpty path)
  where
    pathT = toTextIgnore path
    path' = unpack pathT

    go status
      | isDirectory status =
        foldM (\(y, ys) x -> do
                  (x',xs') <- gatherSizes opts (curDepth + 1) (collapse x)
                  let x''  = y <> x'
                      xs'' = if curDepth < depth opts
                             then ys <> DL.singleton x' <> xs'
                             else DL.empty
                  return $! x'' `seq` xs'' `seq` (x'', xs''))
              (newEntry path True, DL.empty) =<< listDirectory path

      | (isRegularFile status
         && not (annex opts && ".git/annex/" `isInfixOf` pathT))
        || (annex opts && isSymbolicLink status) = do
        status' <-
          -- If status is for a symbolic link, it must be a Git-annex'd file
          if isSymbolicLink status
          then do
            destPath <- readSymbolicLink path'
            if ".git/annex/" `L.isInfixOf` destPath
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

        let fsize     = fileSize status'
            blksize   = fileBlockSize (unsafeCoerce status')
            allocSize = if apparent opts
                        then fromIntegral fsize
                        else fromIntegral blksize * blockSize opts

        return (EntryInfo { _entryPath       = path
                          , _entryCount      = 1
                          , _entryAllocSize  = allocSize
                          , _entryIsDir      = False }, DL.empty)

      | otherwise = returnEmpty path

-- Main.hs (sizes) ends here
