# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Advent of Code 2024 solutions in Haskell, built with Stack on Windows. Each day is a separate module exposing two solution functions (`dayNN_part1`, `dayNN_part2`), both conforming to the signature `String -> IO [Int]` where the input is the raw puzzle text and the result is a list of integers (usually a singleton; some days return multiple values, and an empty list signals "not implemented" to the test runner).

## Common commands

All commands run from the repository root and use `stack` (Windows, bash shell).

```bash
stack build                                          # build the library + executable + tests
stack test                                           # run the full HUnit suite in test/Main.hs
stack exec AdventOfCode2024 -- dayNN_partM path      # run a single day/part against an input file
```

Examples:

```bash
stack exec AdventOfCode2024 -- day01_part1 input/day01-input.txt
stack exec AdventOfCode2024 -- day12_part2 input/day12-input-example.txt
```

The executable times each run and prints `Execution time: ...` after the answer.

There is no single-test selector wired up; `stack test` runs the whole `TestList` in [test/Main.hs](test/Main.hs). To iterate on one day, either comment out unrelated entries in `tests` or just rerun the executable against a specific input.

## Architecture

- [app/Main.hs](app/Main.hs) is a thin dispatcher: it parses `<day_partN> <inputFile>`, looks the name up in `dayTable` (one entry per part, hand-maintained alongside the imports), reads the input file, and invokes the solution inside `timeIt`.
- [src/DayNN.hs](src/) — one module per day, each exporting exactly `dayNN_part1` and `dayNN_part2`. Every module is listed under `library.exposed-modules` in [package.yaml](package.yaml); adding a new day requires an entry there, plus imports and a `dayTable` row in `app/Main.hs`, plus a test case in `test/Main.hs`.
- [test/Main.hs](test/Main.hs) — HUnit tests pinned to the known answers for the real puzzle inputs in `input/`. Tests read input via `unsafePerformIO` inside `run`. Days whose part2 is unsolved use `assertNotImplemented`, which expects the solution to return `[]` (so stubs should return `[]`, not error).
- [src/Lib.hs](src/Lib.hs) is a leftover stub from `stack init` and is unused.
- [input/](input/) holds both the real puzzle inputs (`dayNN-input.txt`) and example inputs from the problem statement (`dayNN-input-example[-N].txt`). These are the fixtures the tests and the CLI read from.

## Conventions

- Solutions return `IO [Int]` even when the answer is a single number — wrap it as `return [answer]`. Day 17 part 1 and Day 18 part 2 return multi-element lists; the test harness compares them element-wise.
- A known-unsolved part should return `[]` and be registered with `assertNotImplemented` in the test file; returning a non-empty wrong answer will fail that assertion with a specific "assumed unimplemented but returned ..." message.
- `ghc-options` in `package.yaml` enables a strict warning set (`-Wall -Wcompat -Wincomplete-uni-patterns -Wmissing-export-lists` etc.); keep new modules warning-clean, and explicitly list exports in the module header.
- Extra dependency `astar-0.3.0.0` is pulled via `extra-deps` in [stack.yaml](stack.yaml) (used by Day 16); add further pins there, not in `package.yaml`.
