{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Applicative
import           Control.Concurrent.ParallelIO
import           Control.Lens
import           Control.Monad hiding (sequence)
import           Data.Function
import qualified Data.List as L
import           Data.Monoid
import           Data.Text as T hiding (filter, map, chunksOf)
import           Filesystem (listDirectory)
import           Filesystem.Path.CurrentOS
import           GHC.Conc
import           Prelude hiding (FilePath, sequence, catch)
import           System.Console.CmdArgs
import           System.Environment (getArgs, withArgs)
import           System.Posix.Files
import           Text.Printf

default (Int, Text)

version :: String
version = "1.0.0"

copyright :: String
copyright = "2012"

sizesSummary :: String
sizesSummary = "sizes v" ++ version ++ ", (C) John Wiegley " ++ copyright

data SizesOpts = SizesOpts { jobs      :: Int
                           , byCount   :: Bool
                           , smalls    :: Bool
                           -- , dirsOnly  :: Bool
                           , recurse   :: Bool
                           , dirs      :: [String] }
               deriving (Data, Typeable, Show, Eq)

sizesOpts :: SizesOpts
sizesOpts = SizesOpts
    { jobs     = def &= name "j" &= typ "INT"
                     &= help "Run INT parallel jobs at once (def: # of procs)"
    , byCount  = def &= typ "BOOL"
                     &= help "Sort output from smallest count to largest"
    , smalls   = def &= typ "BOOL"
                     &= help "Also show small (<1M && <100 files) entries"
    -- , dirsOnly = def &= typ "BOOL"
    --                  &= help "Show directories only"
    , recurse  = def &= typ "BOOL"
                     &= help "Report all entries recursively"
    , dirs     = def &= args &= typDir } &=
    summary sizesSummary &=
    program "sizes" &=
    help "Calculate amount of disk used by the given directories"

data EntryInfo = EntryInfo { _entryPath  :: !FilePath
                           , _entryCount :: !Int
                           , _entrySize  :: !Int }
             deriving Show

newEntry :: FilePath -> EntryInfo
newEntry p = EntryInfo p 0 0

makeLenses ''EntryInfo

main :: IO ()
main = do
  mainArgs <- getArgs
  opts     <- withArgs (if L.null mainArgs then ["--help"] else mainArgs)
                       (cmdArgs sizesOpts)

  -- Set the default concurrency to equal the number of processors available
  caps <- GHC.Conc.getNumProcessors
  _    <- GHC.Conc.setNumCapabilities $ case jobs opts of 0 -> caps; x -> x

  runSizes opts

runSizes :: SizesOpts -> IO ()
runSizes opts = do
  reportSizes opts $ map (fromText . pack) (dirs opts)
  stopGlobalPool

parMap :: (a -> IO b) -> [a] -> IO ()
parMap f xs = parallel (map f xs) >> return ()

reportSizes :: SizesOpts -> [FilePath] -> IO ()
reportSizes opts xs = do
  entryInfos <- parallel $ map (gatherSizes (recurse opts)) xs

  let infos  = map fst entryInfos ++ mconcat (map snd entryInfos)
      sorted = L.sortBy ((compare `on`) $ if byCount opts
                                          then (^. entryCount)
                                          else (^. entrySize)) infos
  forM_ sorted $ \entry ->
    when (smalls opts || entry^.entrySize >= 10 * 1024^2) $
      reportEntry entry

toTextIgnore :: FilePath -> Text
toTextIgnore p = case toText p of Left _ -> ""; Right x -> x

humanReadable :: Int -> String
humanReadable x
  | x < 1024   = printf "%db" x
  | x < 1024^2 = printf "%.0fK" (fromIntegral x / (1024 :: Double))
  | x < 1024^3 = printf "%.1fM" (fromIntegral x / (1024^2 :: Double))
  | x < 1024^4 = printf "%.2fG" (fromIntegral x / (1024^3 :: Double))
  | x < 1024^5 = printf "%.3fT" (fromIntegral x / (1024^4 :: Double))
  | x < 1024^6 = printf "%.3fP" (fromIntegral x / (1024^5 :: Double))
  | otherwise  = error "Too large"

reportEntry :: EntryInfo -> IO ()
reportEntry entry =
  printf (unpack "%10s %10d %s\n")
         (humanReadable (entry^.entrySize))
         (entry^.entryCount)
         (unpack (toTextIgnore (entry^.entryPath)))

gatherSizes :: Bool -> FilePath -> IO (EntryInfo, [EntryInfo])
gatherSizes aggregate path = do
  status <- getSymbolicLinkStatus (unpack $ toTextIgnore path)
  if isSymbolicLink status
    then return (newEntry path, [])
    else
    if isDirectory status
      then do
        files   <- listDirectory path
        entries <- parallel $
                   map (gatherSizes aggregate . collapse) files

        let firsts = map fst entries
        return ( L.foldl' (\current entry ->
                            entryCount +~ entry^.entryCount $
                            entrySize  +~ entry^.entrySize  $ current)
                          (newEntry $ fromText $ toTextIgnore path <> "/")
                          firsts
               , if aggregate
                 then firsts ++ L.concat (map snd entries)
                 else firsts )

      else
      if isRegularFile status
        then let size = fromIntegral (fileSize status) in
             return (entryCount .~ 1 $ entrySize .~ size $ newEntry path, [])
        else return (newEntry path, [])

-- Main.hs (sizes) ends here
