{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception (bracket)
import Control.Monad (unless)
import qualified Data.List as List
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Sizes (
    EntryInfo (..),
    combineEntryResults,
    crossesFileSystemBoundary,
    emptyReportEntries,
    humanReadable,
    reportEntriesToList,
 )
import System.Directory (createDirectory, getTemporaryDirectory, removeFile, removePathForcibly)
import System.Exit (ExitCode (ExitSuccess), exitFailure)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Posix.Types (DeviceID)
import System.Process (readProcessWithExitCode)

main :: IO ()
main = do
    passed <- checkParallel $$(discover)
    unless passed exitFailure

-- | Allocate a unique scratch directory and remove it after the test.
withScratchDirectory :: (FilePath -> IO a) -> IO a
withScratchDirectory = bracket create removePathForcibly
  where
    create = do
        temporaryRoot <- getTemporaryDirectory
        (path, handle) <- openTempFile temporaryRoot "sizes-test"
        hClose handle
        removeFile path
        createDirectory path
        pure path

-- | Generate an EntryInfo with random count and size.
genEntryInfo :: Gen EntryInfo
genEntryInfo = do
    count <- Gen.int (Range.linear 0 10000)
    size <- Gen.int (Range.linear 0 (1024 * 1024 * 1024))
    isDir <- Gen.bool
    pure $ mempty{_entryCount = count, _entryAllocSize = size, _entryIsDir = isDir}

-- humanReadable: bytes
prop_humanReadable_bytes :: Property
prop_humanReadable_bytes =
    withTests 1 . property $
        humanReadable 500 1024 === "500b"

-- humanReadable: kilobytes
prop_humanReadable_kilobytes :: Property
prop_humanReadable_kilobytes =
    withTests 1 . property $
        humanReadable 2048 1024 === "2K"

-- humanReadable: megabytes (base-2)
prop_humanReadable_megabytes :: Property
prop_humanReadable_megabytes =
    withTests 1 . property $
        humanReadable (5 * 1024 * 1024) 1024 === "5.0M"

-- humanReadable: gigabytes (base-2)
prop_humanReadable_gigabytes :: Property
prop_humanReadable_gigabytes =
    withTests 1 . property $
        humanReadable (2 * 1024 * 1024 * 1024) 1024 === "2.00G"

-- humanReadable: megabytes (base-10)
prop_humanReadable_base10 :: Property
prop_humanReadable_base10 =
    withTests 1 . property $
        humanReadable 5000000 1000 === "5.0M"

-- Semigroup: count accumulation is associative
prop_semigroup_count_associative :: Property
prop_semigroup_count_associative = property $ do
    a <- forAll genEntryInfo
    b <- forAll genEntryInfo
    c <- forAll genEntryInfo
    _entryCount ((a <> b) <> c) === _entryCount (a <> (b <> c))

-- Semigroup: size accumulation is associative
prop_semigroup_size_associative :: Property
prop_semigroup_size_associative = property $ do
    a <- forAll genEntryInfo
    b <- forAll genEntryInfo
    c <- forAll genEntryInfo
    _entryAllocSize ((a <> b) <> c) === _entryAllocSize (a <> (b <> c))

-- Monoid: right identity preserves count
prop_monoid_right_identity_count :: Property
prop_monoid_right_identity_count = property $ do
    e <- forAll genEntryInfo
    _entryCount (e <> mempty) === _entryCount e

-- Monoid: right identity preserves size
prop_monoid_right_identity_size :: Property
prop_monoid_right_identity_size = property $ do
    e <- forAll genEntryInfo
    _entryAllocSize (e <> mempty) === _entryAllocSize e

-- Wide directory aggregation must not require stack proportional to entry count.
prop_wide_directory_aggregation_stack_safe :: Property
prop_wide_directory_aggregation_stack_safe =
    withTests 1 . property $ do
        let entryCount = 200000
            leaf = mempty{_entryCount = 1, _entryAllocSize = 2}
            step aggregate _ =
                combineEntryResults
                    True
                    aggregate
                    (leaf, emptyReportEntries)
            (total, reports) =
                List.foldl'
                    step
                    (mempty, emptyReportEntries)
                    [1 .. entryCount]
        _entryCount total === entryCount
        _entryAllocSize total === 2 * entryCount
        length (reportEntriesToList reports) === entryCount

-- Report accumulation preserves sibling/preorder content and drops it past depth.
prop_report_entries_preserve_preorder :: Property
prop_report_entries_preserve_preorder =
    withTests 1 . property $ do
        let first = mempty{_entryCount = 1, _entryAllocSize = 10}
            child = mempty{_entryCount = 2, _entryAllocSize = 20}
            grandchild = mempty{_entryCount = 3, _entryAllocSize = 30}
            firstResult =
                combineEntryResults
                    True
                    (mempty, emptyReportEntries)
                    (first, emptyReportEntries)
            (_, grandchildReports) =
                combineEntryResults
                    True
                    (mempty, emptyReportEntries)
                    (grandchild, emptyReportEntries)
            (_, retained) =
                combineEntryResults
                    True
                    firstResult
                    (child, grandchildReports)
            (_, dropped) =
                combineEntryResults
                    False
                    firstResult
                    (child, grandchildReports)
            entryIdentity entry = (_entryCount entry, _entryAllocSize entry)
        fmap entryIdentity (reportEntriesToList retained)
            === [(1, 10), (2, 20), (3, 30)]
        reportEntriesToList dropped === []

-- The packaged executable traverses a real nested tree in report preorder.
prop_cli_traverses_nested_directory :: Property
prop_cli_traverses_nested_directory =
    withTests 1 . property $ do
        (root, child, file, exitCode, stdout, stderr) <- evalIO $
            withScratchDirectory $ \root -> do
                let child = root </> "child"
                    file = child </> "file"
                createDirectory child
                writeFile file "abc"
                (exitCode, stdout, stderr) <-
                    readProcessWithExitCode
                        "sizes"
                        ["-j1", "-a", "-s", "-d3", root]
                        ""
                pure (root, child, file, exitCode, stdout, stderr)
        annotate stderr
        exitCode === ExitSuccess
        fmap (last . words) (lines stdout)
            === [root ++ "/", child ++ "/", file]

-- humanReadable always returns a non-empty string
prop_humanReadable_nonempty :: Property
prop_humanReadable_nonempty = property $ do
    x <- forAll $ Gen.int (Range.linear 0 (1024 * 1024 * 1024 * 1024))
    d <- forAll $ Gen.element [1000, 1024]
    assert $ not (null (humanReadable x d))

-- | Generate an arbitrary device ID.
genDevice :: Gen DeviceID
genDevice = fromIntegral <$> Gen.int (Range.linear 0 100000)

-- crossesFileSystemBoundary: with the option disabled, nothing is ever a crossing
prop_oneFS_disabled_never_crosses :: Property
prop_oneFS_disabled_never_crosses = property $ do
    root <- forAll $ Gen.maybe genDevice
    dev <- forAll genDevice
    crossesFileSystemBoundary False root dev === False

-- crossesFileSystemBoundary: the traversal root only establishes the boundary
prop_oneFS_root_establishes_boundary :: Property
prop_oneFS_root_establishes_boundary = property $ do
    dev <- forAll genDevice
    crossesFileSystemBoundary True Nothing dev === False

-- crossesFileSystemBoundary: same device as the root is not a crossing
prop_oneFS_same_device_stays :: Property
prop_oneFS_same_device_stays = property $ do
    dev <- forAll genDevice
    crossesFileSystemBoundary True (Just dev) dev === False

-- crossesFileSystemBoundary: a different device than the root is a crossing
prop_oneFS_different_device_crosses :: Property
prop_oneFS_different_device_crosses = property $ do
    root <- forAll genDevice
    delta <- forAll $ Gen.int (Range.linear 1 100000)
    let dev = root + fromIntegral delta
    crossesFileSystemBoundary True (Just root) dev === True
