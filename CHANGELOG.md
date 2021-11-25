# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.57] - 2021-11-25

- makes cycle parsing less awful

## [0.0.56] - 2021-11-22

- adds lenses for `Either'` and `Maybe'`

## [0.0.55] - 2021-11-22

- _even_ more duck typing for tidal objects and utility functions.

## [0.0.54] - 2021-11-22

- more duck typing for tidal objects.

## [0.0.53] - 2021-11-22

- updates to wags 0.6.0.

## [0.0.52] - 2021-11-16

- adds a residual and an analyser to tidal.

## [0.0.51] - 2021-11-15

- adds a `doDownloads'` function for arbitrary mutable refs, ie coming from react's `useRef`.

## [0.0.50] - 2021-11-12

- adds cycle duration as a settable parameter to future, smoothing out a bug where "silent" futures had hardcoded internal durations.

## [0.0.49] - 2021-11-11

- enlarges scope of downloader to deal with complex futures.

## [0.0.48] - 2021-11-11

- adds behavior-like information to unlock the future in the form of `{ clockTime :: Number }`.

## [0.0.47] - 2021-11-10

- makes pitch use `Identity` instead of `Function Number`. In the beginning, we may want to rethink the representations entirely and make them not functorial, kicking the functorial bits elsewhere into the stack.

## [0.0.46] - 2021-11-10

- uses homogeneous for unwrapCofree.

## [0.0.45] - 2021-11-10

- uses homogeneous in place of heterogeneous when homogeneous will do.

## [0.0.44] - 2021-11-08

- adds Gamelan sound lib.
- adds arbitrary pitch.

## [0.0.43] - 2021-11-03

- renames oscillator package.

## [0.0.42] - 2021-11-02

- slightly lessens `longest` duration.

## [0.0.41] - 2021-11-02

- adds `longest` for long duration.

## [0.0.40] - 2021-11-01

- updates to newest `wags` and changes download syntax.

## [0.0.39] - 2021-10-30

- makes playback of single note longer.

## [0.0.38] - 2021-10-25

- improves Safari and iOS playback.

## [0.0.37] - 2021-10-25

- adds a player for simple on-off playback.

## [0.0.36] - 2021-10-25

- adds miniplay for small player environments.

## [0.0.35] - 2021-10-25

- removes volume dip based on duration in tidal.

## [0.0.34] - 2021-10-25

- adds tidal-esque syntax to project. This allows `wagsi` to depend on `wags-lib` and allows for the `trypurescript` instance with `wags-lib` to use mini-notation and the tidal-like engine.

## [0.0.33] - 2021-10-05

- updates to new wags.

## [0.0.32] - 2021-10-05

- updates to new wags.

## [0.0.31] - 2021-10-05

- updates to new wags.

## [0.0.30] - 2021-10-02

- updates to new wags.

## [0.0.29] - 2021-09-25

- uses function of time as default for pitch.

## [0.0.28] - 2021-09-25

- updates learn to use applicatives.

## [0.0.27] - 2021-09-25

- adds an input parameter to the score generator.

## [0.0.26] - 2021-09-24

- adds a loading alert to the load page.

## [0.0.25] - 2021-09-24

- adds tidal example.

## [0.0.24] - 2021-09-23

- updates essential dependencies.

## [0.0.23] - 2021-09-21

- bugfix for rests in sequences.

## [0.0.22] - 2021-09-21

- rewrites rests in the learn DSL.

## [0.0.21] - 2021-09-21

- adds blue danube waltz example.

## [0.0.20] - 2021-09-20

- adds musical dictionary to the wags learn DSL.

## [0.0.19] - 2021-09-19

- adds styling to play component.

## [0.0.18] - 2021-09-18

- adds lazy list version of cycle.

## [0.0.17] - 2021-09-15

- adds learn library.

## [0.0.16] - 2021-09-05

- upgrades to newest wags.

## [0.0.15] - 2021-09-05

- upgrades to newest wags.
- renames sector to gamelan.

## [0.0.14] - 2021-08-22

- adds sector example.
- upgrades to newest wags.
- reduces number of computations.

## [0.0.13] - 2021-07-15

### Added

- `heads` and `tails` recursively apply to records.

## [0.0.12] - 2021-07-14

### Added

- More polymorphism to terraced piecewise functions, allowing for any type implementing `Eq` to be used as the co-domain.

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