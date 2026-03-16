{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (unless)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Sizes (EntryInfo (..), humanReadable)
import System.Exit (exitFailure)

main :: IO ()
main = do
    passed <- checkParallel $$(discover)
    unless passed exitFailure

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

-- humanReadable always returns a non-empty string
prop_humanReadable_nonempty :: Property
prop_humanReadable_nonempty = property $ do
    x <- forAll $ Gen.int (Range.linear 0 (1024 * 1024 * 1024 * 1024))
    d <- forAll $ Gen.element [1000, 1024]
    assert $ not (null (humanReadable x d))
