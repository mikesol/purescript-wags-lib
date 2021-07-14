# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.11] - 2021-07-14

### Added

- A latch function that emits `Just value` when there's a change and otherwise emits `Nothing`.

## [0.0.10] - 2021-07-13

### Added

- Left-biased and right-biased terraced piecewise functions, meaning there is no interpolation.

## [0.0.9] - 2021-07-11

### Added

- Adds `Trigger` that triggers the clock to restart whenever there is a ping. If the ping is in the future (meaning if there is an `offset`), the clock will restart in the negative values.

## [0.0.8] - 2021-07-11

### Added

- Revamps newtypes to implement `Comonad` and `ComonadCofree`, allowing for better type inference and removing boilerplate.

## [0.0.7] - 2021-07-10

### Removed

- Makes tighter restriction on `Actualized` and `ActualizeMany'`, allowing for more powerful fundeps.

## [0.0.6] - 2021-07-10

### Removed

- `fromTemplate` is now part of `purescript-wags`.

## [0.0.5] - 2021-07-03

### Added

- Semigroup and Monoid instances for many newtypes around Cofree Comonads.
- Functions for working with rows of comonads.
- Functions for working with templates of audio units.

## [0.0.4] - 2021-07-03

### Added

- A function `blip` that only changes when there is a change from 0 to 1.

## [0.0.3] - 2021-07-03

### Added

- `bufferPool` now takes a `rest` parameter that is passed through to the individual vector element at instantiation time.
- supports arbitrary streams via `stream`, `deadEnd` and `cycle`.

## [0.0.2] - 2021-07-03

### Changed

- Removes polykinds due to compiler bug

## [0.0.1] - 2021-07-03

### Changed

- Moves BufferPool to WAGS.Lib.BufferPool

## [0.0.0] - 2021-07-03

### Added

- Utilities for working with piecewise functions of time.
- Utilities for working with buffer pools.
- Utilities for working with rates and emitters.