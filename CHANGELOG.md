# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.3] - 2021-03-07

### Added

- `bufferPool` now takes a `rest` parameter that is passed through to the individual vector element at instantiation time.
- supports arbitrary streams via `stream`, `deadEnd` and `cycle`.

## [0.0.2] - 2021-03-07

### Changed

- Removes polykinds due to compiler bug

## [0.0.1] - 2021-03-07

### Changed

- Moves BufferPool to WAGS.Lib.BufferPool

## [0.0.0] - 2021-03-07

### Added

- Utilities for working with piecewise functions of time.
- Utilities for working with buffer pools.
- Utilities for working with rates and emitters.