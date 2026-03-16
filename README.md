# sizes

I wrote `sizes` because `du` wasn't giving me what I needed. When you're
managing large filesystems -- especially ones with git-annex repositories --
you want to see where your disk space is actually going, sorted by size or
file count, with the ability to exclude paths and handle hard-linked files
properly.

It's `du` with a bit more flexibility: concurrent traversal, PCRE path
exclusion, git-annex awareness, hard link deduplication, and output that
focuses on what matters.

## Getting started

```bash
# With Nix (preferred)
nix build github:jwiegley/sizes
./result/bin/sizes /path/to/dir

# Or install via Cabal
cabal install sizes
```

## Usage

```bash
# Basic: show directories using >10MB
sizes /path/to/dir

# Sort by file count
sizes -c /path/to/dir

# Use 4 concurrent threads
sizes -j4 /path/to/dir

# Exclude paths matching a regex
sizes -x '\.cache' /path/to/dir

# Handle git-annex repos correctly
sizes -A /path/to/annex-repo

# Deduplicate hard links
sizes -L /path/to/dir

# Show apparent sizes (not allocated blocks)
sizes --apparent /path/to/dir

# Base-10 units (MB/GB instead of MiB/GiB)
sizes -H /path/to/dir

# Lower the minimum threshold
sizes -m 1 /path/to/dir

# Increase reporting depth
sizes --depth 3 /path/to/dir

# Show everything, including small entries
sizes -s /path/to/dir
```

## Options

| Flag | Description |
|------|-------------|
| `-j INT` | Concurrent threads (default: 2) |
| `-c` | Sort by file count |
| `-A` | Git-annex aware |
| `--apparent` | Apparent sizes, not disk blocks |
| `-H` | Base-10 (MB/GB) |
| `-x REGEX` | Exclude matching paths |
| `-m INT` | Minimum size in MB (default: 10) |
| `-M INT` | Minimum file count (default: 100) |
| `-B INT` | Block size in bytes (default: 512) |
| `-s` | Show small entries |
| `-L` | Deduplicate hard links |
| `--depth INT` | Reporting depth (default: 1) |

## Development

```bash
# Enter dev shell
nix develop

# Build
cabal build

# Run tests
cabal test

# Format code
fourmolu --mode inplace Sizes.hs app/Main.hs test/Spec.hs

# Lint
hlint Sizes.hs app/Main.hs test/Spec.hs

# Run all checks
nix flake check
```

## License

BSD-3-Clause -- see [LICENSE.md](LICENSE.md).
