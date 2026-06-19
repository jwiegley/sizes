# Design: `--one-file-system` / `-X`

Date: 2026-06-18
Author: John Wiegley (with Claude Code)
Status: Approved

## Goal

Add an option to `sizes` that prevents traversal from crossing filesystem
boundaries, so that running `sizes` on a tree containing other mounted
filesystems (including NFS and SMB/Samba mounts) confines the calculation to a
single physical filesystem. This mirrors `du -x` / `du --one-file-system`,
`tar --one-file-system`, and `find -xdev`.

## Flag

- Long: `--one-file-system` (canonical name used by `du`, `tar`, `rsync`)
- Short: `-X` (`du`'s short `-x` is already taken by the exclude-regex option)
- Default: off; when off, behavior is byte-for-byte identical to today.

cmdargs wiring uses `&= explicit` with both names so cmdargs does not also
auto-derive `--onefilesystem` from the field name:

```haskell
, oneFileSystem =
    def
        &= explicit
        &= name "one-file-system"
        &= name "X"
        &= typ "BOOL"
        &= help "Do not descend into directories on other filesystems"
```

## Semantics

A filesystem boundary is exactly a change in the device ID (`st_dev`) reported
by `stat`/`lstat`. The rule:

- Each directory given on the command line establishes its **own** root device
  (matching `du -x`'s per-argument behavior).
- During recursion, any entry whose device differs from its traversal root's
  device is skipped entirely.
- A mount point — and everything beneath it — is therefore skipped, because the
  mount root's device differs from its parent's. This covers local mounts as
  well as NFS and SMB/Samba mounts.

## Implementation (`Sizes.hs`)

1. Add `oneFileSystem :: Bool` to `SizesOpts` and wire the flag as above.

2. Pure, unit-testable decision function (exported from `Sizes`):

   ```haskell
   crossesFileSystemBoundary
       :: Bool            -- ^ is --one-file-system enabled?
       -> Maybe DeviceID  -- ^ root device (Nothing at the top-level argument)
       -> DeviceID        -- ^ device of the current entry
       -> Bool
   crossesFileSystemBoundary False _        _   = False
   crossesFileSystemBoundary True  Nothing  _   = False  -- root establishes boundary
   crossesFileSystemBoundary True  (Just r) dev = dev /= r
   ```

3. Thread a `Maybe DeviceID` (the root device) through `gatherSizes`:
   - The top-level call from `reportSizesForDir` passes `Nothing`.
   - After `status` is obtained, if
     `crossesFileSystemBoundary (oneFileSystem opts) mRootDev (deviceID status)`
     is `True`, return empty (skip the entry and its subtree).
   - Otherwise descend, passing `Just (deviceID status)` to child calls. Because
     a non-skipped directory always shares the root device, passing the current
     directory's device down is equivalent to threading the original root.
   - The check sits after the existing exclude check and reuses the
     already-fetched `status`, so **no new syscalls** are introduced
     (`gatherSizes` already reads `deviceID` for hard-link dedupe).

## Interactions

Independent of `--exclude`, `--depth`, `--annex`, and `-L` (hard-link dedupe).
Annex symlinks are evaluated by the symlink's own device (which equals the
root), so following annex content is unaffected by this option.

## Testing

- Unit-test `crossesFileSystemBoundary` with hedgehog (the four logical cases:
  option off; option on at root; same device; different device).
- The traversal wiring requires real mount points to exercise, which is not
  practical in CI; verify manually (e.g. `sizes -X /` versus `sizes /`, or over
  a directory containing a mount).

## Docs

Update `README.md` and the options list in `CLAUDE.md`.
