# Frozen Plan: Home-directory stack overflow

Date: 2026-07-14
Status: Frozen for this Wiggum run

## Objective

Resolve the stack-space overflow produced when `sizes` is run from the home
directory as:

```console
sudo sizes -aHLX -d9 -x Library/CloudStorage
```

## Definition of Done

1. The failure is reproduced or otherwise tied to authoritative evidence from
   the executable selected by `sudo`, and its root cause is identified.
2. The repository contains a durable fix for that root cause. The fix must not
   depend on the user remembering an ad hoc `+RTS -K... -RTS` suffix.
3. A regression test or equivalent automated gate fails for the original
   condition and passes with the fix.
4. Formatting, linting, the build, and the complete test suite pass through the
   worktree's direnv environment.
5. The user-visible executable selected by `sudo sizes` contains the fix, and
   the original command run from the home directory completes without the
   reported stack overflow on the current filesystem snapshot.
6. The final work commit passes an independent audit, no actionable partner
   observation remains, the branch is locally current with its base, and the
   worktree is clean.

These criteria are read-only for the purpose of this run and must not be
weakened to fit the implementation.
