# Wiggum Handoff: Home-directory stack overflow

Updated: 2026-07-14

## Current state

- Objective and done-criteria are frozen in `WIGGUM-PLAN.md`.
- The fix makes numeric aggregation strict, replaces left-associated `DList`
  composition with `Data.Sequence`, and restores GHC's platform stack default.
- The expanded suite has 17 passing properties, including the 200,000-entry
  small-stack regression, exact nested report order, and a packaged-CLI
  filesystem traversal.
- The full pre-commit hook passes Fourmolu, HLint, the Nix package build, all
  flake checks, the linked-artifact RTS check, and the test suite.
- `~/.local/bin/sizes` is installed and selected by both a clean login shell
  and `sudo`; the literal original command completes with exit 0.
- Two independent `fess` audits found no core algorithm defect. Their real CI,
  coverage, evidence, and documentation findings have been fixed.
- `origin/master` was fetched after the work commit; the branch is one commit
  ahead and zero behind, with no partner-observation directory.
- Anvil is backed by a dedicated Emacs daemon. Its buffer view does not cover a
  separate interactive Emacs session.
- PAL tools are not advertised on this host, so no PAL consensus is available.
- The worktree environment must be entered with `direnv exec .`; do not use
  `nix develop` or install dependencies on the fly.

## Next action

Amend the work commit with the verified audit fixes, rerun its full hook, and
perform the final independent audit. No implementation or acceptance work
remains.

## Stop-and-escalate attempt counts

- Repeated failing signature: 0/3
- Unusable subagent output: 0/2

## Running learnings

- GHC's `current size 33608 bytes` overflow text is a known misleading error
  report and does not contradict the old binary's embedded 64 MiB maximum.
- The obsolete production `-K64M` was a restrictive maximum, not an increased
  initial stack allocation.
- Lazy numeric totals and left-associated difference-list composition provided
  independent linear-stack paths in the original traversal.
- No separate append-only journal is in use for this run.

## Verification evidence

- Baseline installed artifact:
  - `sudo which sizes` selected `/etc/profiles/per-user/johnw/bin/sizes`.
  - `sudo sizes +RTS --info -RTS` reported `Flag -with-rtsopts: -K64M`.
  - `sudo sizes +RTS -K128M -RTS --help` was rejected because most RTS options
    were disabled.
  - The literal home command exited 2 with the reported stack overflow.
- Red/green regression:
  - Before the strict/sequence change, `cabal test all` overflowed in the new
    wide-directory property and reported 0 of 1 test suites passed.
  - After the fix and audit coverage additions, the same command reports all
    17 properties and the test suite passing.
  - The linked test executable's `+RTS --info` reports
    `Flag -with-rtsopts: -K1M`, proving the test-only fault injection.
- Build and artifact gates:
  - `fourmolu --mode check`, HLint, and `cabal build all` pass.
  - The pre-commit hook completed in 1411.97 seconds with all four commands
    green; its full flake check built `sizes-rts-defaults-check` and ran the
    Haskell test check.
  - The corrected Cabal executable reports an empty `Flag -with-rtsopts`.
- Installed acceptance:
  - `cabal install exe:sizes --installdir=$HOME/.local/bin` completed.
  - A clean login reports both `command -v sizes` and `sudo which sizes` as
    `/Users/johnw/.local/bin/sizes`.
  - The installed executable reports an empty `Flag -with-rtsopts`.
  - From `/Users/johnw`, the literal
    `sudo sizes -aHLX -d9 -x Library/CloudStorage` completed with exit 0.
