{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Concurrent.ParallelIO
import           Control.DeepSeq
import           Control.Exception
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
                           , depth     :: Int
                           , dirs      :: [String] }
               deriving (Data, Typeable, Show, Eq)

sizesOpts :: SizesOpts
sizesOpts = SizesOpts
    { jobs     = def &= name "j" &= typ "INT"
                     &= help "Run INT concurrent finds at once (default: 2)"
    , byCount  = def &= typ "BOOL"
                     &= help "Sort output by count (default: by size)"
    , smalls   = def &= typ "BOOL"
                     &= help "Also show small (<1M && <100 files) entries"
    -- , dirsOnly = def &= typ "BOOL"
    --                  &= help "Show directories only"
    , depth    = def &= typ "INT"
                     &= help "Report all entries to a depth of INT (default: 0)"
    , dirs     = def &= args &= typ "DIRS..." } &=
    summary sizesSummary &=
    program "sizes" &=
    help "Calculate amount of disk used by the given directories"

data EntryInfo = EntryInfo { _entryPath  :: !FilePath
                           , _entryCount :: !Int
                           , _entrySize  :: !Int
                           , _entryIsDir :: !Bool }
             deriving Show

instance NFData EntryInfo where
  rnf a = a `seq` ()

newEntry :: FilePath -> Bool -> EntryInfo
newEntry p isDir = EntryInfo p 0 0 isDir

makeLenses ''EntryInfo

main :: IO ()
main = do
  mainArgs <- getArgs
  opts     <- withArgs (if L.null mainArgs then ["."] else mainArgs)
                       (cmdArgs sizesOpts)
  _        <- GHC.Conc.setNumCapabilities $ case jobs opts of 0 -> 2; x -> x
  runSizes opts

runSizes :: SizesOpts -> IO ()
runSizes opts = do
  reportSizes opts $ map (fromText . pack) (dirs opts)
  stopGlobalPool

parMap :: (a -> IO b) -> [a] -> IO ()
parMap f xs = parallel (map f xs) >> return ()

reportSizes :: SizesOpts -> [FilePath] -> IO ()
reportSizes opts xs = do
  entryInfos <- parallel $ map (gatherSizesW (depth opts) 0) xs
  let infos  = map fst entryInfos ++ mconcat (map snd entryInfos)
      sorted = L.sortBy ((compare `on`) $ if byCount opts
                                          then (^. entryCount)
                                          else (^. entrySize)) infos
  forM_ sorted $ \entry ->
    when ( smalls opts
         || entry^.entrySize >= 10 * 1024^2
         || entry^.entryCount > 100)
         $ reportEntry entry

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
  let path = unpack . toTextIgnore $ entry^.entryPath
  in printf (unpack "%10s %10d %s%s\n")
            (humanReadable (entry^.entrySize)) (entry^.entryCount) path
            (unpack $ if entry^.entryIsDir && L.last path /= '/'
                      then "/" else "")

toTextIgnore :: FilePath -> Text
toTextIgnore p = case toText p of Left _ -> ""; Right x -> x

gatherSizesW :: Int -> Int -> FilePath -> IO (EntryInfo, [EntryInfo])
gatherSizesW m d p =
  catch (gatherSizes m d p)
        (\e -> do
            putStrLn $ show (e :: IOException)
            return (newEntry p False, []))

gatherSizes :: Int -> Int -> FilePath -> IO (EntryInfo, [EntryInfo])
gatherSizes maxDepth curDepth path = do
  let path' = unpack $ toTextIgnore path
  status <- if curDepth == 0
            then getFileStatus path'
            else getSymbolicLinkStatus path'
  if isDirectory status
    then do
      files   <- listDirectory path
      entries <- let func = gatherSizesW maxDepth (curDepth + 1) . collapse
                 in if curDepth == 0
                    then parallel $ map func files
                    else mapM func files
      let firsts = map fst entries
      return $!! ( L.foldl' (\current entry ->
                           entryCount +~ entry^.entryCount $
                           entrySize  +~ entry^.entrySize  $ current)
                         (newEntry path True)
                         firsts
                 , if curDepth < maxDepth
                   then firsts ++ L.concat (map snd entries)
                   else [] )
    else
    if isRegularFile status
      then return ( entryCount .~ 1 $
                    entrySize .~ fromIntegral (fileSize status) $
                    newEntry path False, [] )
      else return (newEntry path False, [])

-- Main.hs (sizes) ends here
