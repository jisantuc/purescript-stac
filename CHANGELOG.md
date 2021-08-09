# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- Collection item list supports the limit query parameter [#15](https://github.com/jisantuc/purescript-stac/pull/15) (@jisantuc)
- Updated models for compatibility with spec version 1.0.0-rc2 [17](https://github.com/jisantuc/purescript-stac/pull/17) @jisantuc
- Added client tests with a running Franklin server [#18](https://github.com/jisantuc/purescript-stac/pull/18)
- Test core client methods [#23](https://github.com/jisantuc/purescript-stac/pull/23)

### Changed
- Switched to spec discovery to separate client and serde tests [#18](https://github.com/jisantuc/purescript-stac/pull/18)
- Made collection and link decoders more tolerant of missing nullable fields [#18](https://github.com/jisantuc/purescript-stac/pull/18)
- Added Akita agent to CI to document the API endpoints consumed from API traffic [#18](https://github.com/jisantuc/purescript-stac/pull/18)
- Only pass PR flag to Akita agent when not on the `main` branch [#20](https://github.com/jisantuc/purescript-stac/pull/20)

## [1.0.1] - 2021-03-21
### Fixed
- Included trailing slash in collection items route [#14](https://github.com/jisantuc/purescript-stac/pull/14) (@jisantuc)

## 1.0.0 - 2021-03-08
### Changed
- Replaced OneOrBoth custom refinement with These [#8](https://github.com/jisantuc/purescript-stac/pull/8) (@jisantuc)
- Upgraded PureScript to 0.14.0 [#12](https://github.com/jisantuc/purescript-stac/pull/12) (@jisantuc)

### Added
- Added `Item` type [#6](https://github.com/jisantuc/purescript-stac/pull/6) (@jisantuc)
- Implemented core API specification client methods [#10](https://github.com/jisantuc/purescript-stac/pull/10) (@jisantuc)

[Unreleased]: https://github.com/jisantuc/purescript-stac/compare/v1.0.1...HEAD
[1.0.1]: https://github.com/jisantuc/purescript-stac/compare/v1.0.0...v1.0.1
