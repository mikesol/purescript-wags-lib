# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.103] - 2022-03-18

- bumps the wags version

## [0.0.102] - 2022-03-16

- bumps the wags version

## [0.0.101] - 2022-03-15

- bumps the wags version

## [0.0.100] - 2022-03-12

- adds `cofreeTraversal_`
- deletes `feedback` example as it took too long to build (RIP)

## [0.0.99] - 2022-02-28

- fixes bug in `numericTumult`, makes monomorphic-er in the process.

## [0.0.98] - 2022-02-28

- bumps wags version.

## [0.0.97] - 2022-02-15

- adds flatten function.

## [0.0.96] - 2022-02-15

- adds some predefined synths.

## [0.0.95] - 2022-02-15

- fixes change sample functions.

## [0.0.94] - 2022-02-15

- adds `weightedChoice`.

## [0.0.93] - 2022-02-14

- better `changeSampleF`.

## [0.0.92] - 2022-02-14

- smarter signature for forward.

## [0.0.91] - 2022-02-14

- adds numeric tumult.

## [0.0.90] - 2022-02-14

- removes long objects and exports from `Samples` and `Cycles`.
- holds past of tumult.

## [0.0.89] - 2022-02-13

- revamps drone to have second derivative, allowing for simple harmonic motion.
- removes global fader from tracks, as no one seems to use it, and folks use `fx` instead.

## [0.0.88] - 2022-02-13

- changes back derivative to previous implementation and calls it `cderiv`

## [0.0.87] - 2022-02-11

- changes air to water
- adds drone verbs
- uses new wags

## [0.0.86] - 2022-02-11

- adds synth lib

## [0.0.85] - 2022-02-11

- uses type equality for more elegant proof in the `S` class.
- removes unneeded monomorphic functions.
- adds tumult to individual samples.

## [0.0.84] - 2022-02-10

- eliminates specialized `parse_` in favor of polymorphic version.

## [0.0.83] - 2022-02-10

- changes forward to forwardFoT

## [0.0.82] - 2022-02-10

- adds a typelevel tidal parser

## [0.0.81] - 2022-02-09

- adds a syntax for lazy note input

## [0.0.80] - 2022-02-08

- shuffles some types around to be more consistent
- adds `mseq` function for sequential music

## [0.0.80] - 2022-01-11

- shuffles some types around to be more consistent
- adds `mseq` function for sequential music

## [0.0.79] - 2022-01-11

- provides external control to the tidal API

## [0.0.78] - 2022-01-09

- adds simplePiecewise function

## [0.0.77] - 2022-01-02

- starts the first cycle directly on beat 0, avoiding unnecessary silence

## [0.0.76] - 2022-01-01

- uses a behavior for the next cycle instead of an event. This allows for behaviors to preload before a work begins, which improves throughput on start.

## [0.0.75] - 2021-12-27

- drops down to six voices after more extensive testing & out of abundance of caution to decrease the possibility of underflow. more extensive testing is needed to see if the additional voices are in fact the culprit or if it is something else.

## [0.0.74] - 2021-12-27

- adds buffers to tumult to allow for convolution

## [0.0.73] - 2021-12-27

- fix: does downloads for additional tracks

## [0.0.72] - 2021-12-27

- expands tidal to 9-voice maximum - earth, wind, fire, lambert, hendricks, ross, peter, paul, mary

## [0.0.71] - 2021-12-26

- fixes `Semigroup` instance bug

## [0.0.70] - 2021-12-26

- adds `Semigroup` instance for `TheFuture event`


## [0.0.69] - 2021-12-15

- changes internal types to more js-friendly representations for smoother js interop.

## [0.0.68] - 2021-12-05

- avoids using `step` in `Engine.purs`. Because `step` is based on an underlying hot event, whenever it is subscribed to, it will emit that event first. By changing it to a cold event, we get better performance.

## [0.0.67] - 2021-12-05

- changes internal representation of `engine` to use a behavior for the next wag. This takes pressure off the interactivity.

## [0.0.66] - 2021-12-01

- adds an `addEffect` function for easier addition of effect streams.

## [0.0.65] - 2021-11-30

- adds a `drone` function to ease the creation of drones.
- updates drone sound files.

## [0.0.64] - 2021-11-30

- adds setters for rate, volume, and other parameters in the tidal dsl.

## [0.0.63] - 2021-11-30

- updates wags to `v0.6.2`.

## [0.0.62] - 2021-11-28

- adds apply, applicative, bind and monad instances to `Cycle`.

## [0.0.61] - 2021-11-26

- simplifies parser

## [0.0.60] - 2021-11-25

- fixes parsing bug

## [0.0.59] - 2021-11-25

- fixes parsing bug

## [0.0.58] - 2021-11-25

- fixes parsing bug

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
