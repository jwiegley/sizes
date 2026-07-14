# Claude Instructions for sizes

This file contains instructions for Claude Code when working with the sizes codebase.

## Project Overview

`sizes` is a disk usage analyzer written in Haskell that recursively computes space and inode consumption of files and directories. Think `du` with more flexibility.

**Author**: John Wiegley
**Version**: 2.4.3
**GHC Version**: 9.10.2

## Build Commands

### Using Nix (Preferred)
```bash
# Build the project
nix build

# Enter development shell with all tools
nix develop

# Run the binary
./result/bin/sizes [OPTIONS] [DIRS...]
```

### Using Cabal
```bash
# Build
cabal build

# Run
cabal run sizes -- [OPTIONS] [DIRS...]

# Install globally
cabal install --installdir=$HOME/.local/bin
```

## Key Command-Line Options

- `-j INT`: Number of concurrent threads (default: 2)
- `-c`: Sort by file count instead of size
- `-A`: Handle git-annex symlinks specially
- `--apparent`: Show apparent sizes instead of allocated disk blocks
- `-H`: Use base-10 (MB/GB) instead of base-2 (MiB/GiB)
- `-x REGEX`: Exclude paths matching PCRE regex
- `-m INT`: Minimum size to show in MB (default: 10)
- `-M INT`: Minimum file count to show (default: 100)
- `-B INT`: Block size in bytes (default: 512)
- `-s`: Show small entries (<1M and <100 files)
- `-L`: Deduplicate hard links (count each inode only once)
- `-X`, `--one-file-system`: Do not descend into directories on other filesystems
- `--depth INT`: Depth to report (default: 1)

## Architecture

### Three-Layer Design

1. **FFI Layer** (`Stat.hsc`, `HsStat.h`, `HsStat.c`)
   - Provides C bindings to access `st_blocks` from stat structure
   - Uses hsc2hs preprocessor for cross-platform compatibility
   - Key function: `fileBlockSize` retrieves allocated block count

2. **Core Logic** (`Sizes.hs`)
   - `gatherSizes`: Recursive directory traversal with monadic accumulation
   - `EntryInfo`: Monoid for compositional aggregation of directory stats
   - Uses `Data.Sequence` for stack-safe report accumulation during recursion
   - Parallel processing of top-level directories via `parallel-io`

3. **CLI Interface**
   - `cmdargs` library for declarative argument parsing
   - `SizesOpts` data type defines all command options

### Key Design Patterns

- **Monoid Composition**: `EntryInfo` instances combine directory statistics
- **Lens-based Access**: Template Haskell generates lenses for clean field access
- **Strict Totals**: Strict numeric fields and bang patterns prevent count/size thunk chains
- **Finger Trees**: `Data.Sequence` supports stack-safe append and concatenation

### Git-annex Integration

When `--annex` flag is set:
- Symlinks pointing to `.git/annex/` are followed to get actual file sizes
- Regular files in `.git/annex/` are ignored
- This handles git-annex's content-addressed storage correctly

## Known Issues & TODOs

1. **Partial Function** (`reportEntry`): Uses `L.last` without an empty-path check
2. **unsafeCoerce Hack** (`gatherSizes`): Coerces between file-status types for FFI
3. **Error Output** (`gatherSizes`): Recoverable I/O errors print to stdout
4. **Regex Compilation** (`gatherSizes`): Compiles the exclusion regex for every path
5. **Tight Dependencies**: Very narrow version bounds in the Cabal file may cause build issues

## Critical Code Locations

- **Main traversal logic**: `gatherSizes` in `Sizes.hs`
- **Parallel execution setup**: `sizesMain` and `reportSizes` in `Sizes.hs`
- **FFI block size access**: `fileBlockSize` in `Stat.hsc` and `gatherSizes`
- **Git-annex handling**: `gatherSizes` in `Sizes.hs`
- **Entry filtering logic**: `reportEntryP` in `Sizes.hs`
- **Human-readable formatting**: `humanReadable` in `Sizes.hs`

## Performance Considerations

- Accumulates all reportable entries in memory before sorting
- Uses parallel-io for concurrent processing of top-level directories
- Uses GHC's platform-aware stack limit instead of imposing a fixed maximum
- Strict numeric aggregation prevents count/size thunk accumulation
- `Data.Sequence` prevents linear-stack report-list realization

## Dependencies with Specific Concerns

- `cmdargs 0.10-0.11`: Very tight bound, may need relaxing
- `regex-pcre`: Used for path exclusion patterns
- `parallel-io`: Provides work-stealing parallelism
- `lens`: Heavy use throughout for field access
- `unix`: Required for symlink and file status operations

## Development Notes

The codebase shows sophisticated Haskell patterns but maintains readability. When modifying:
- Maintain strict evaluation discipline to prevent space leaks
- Be careful with the FFI layer - the unsafeCoerce is fragile
- Test with deep directory trees and git-annex repositories
- Consider memory usage for very large directory structures
