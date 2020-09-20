# Changelog

All notable changes to this project will be documented in this file.


## [Unreleased]


## [0.2.1] - 2020-09-20
### Added
- Add `eprint!` and `eprintln!`

## [0.2.0] - 2020-09-13
### Breaking changes
- Minimal Rust version bumped to 1.46.0

### Changed
- `syn` dependency removed
- Emit error if arguments are not used

### Fixed
- Implement width and precision non-constant arguments (e.g. `{:0$}` or
  `{:.prec$}` or `{:.*}`)
- Fix named arguments also working as positional ones
- Fix bug in parsing 0 flag in format spec (`{:0$}` now parses as "the width is
  specified in the first argument" instead of the zero flag)


## [0.1.1] - 2020-09-05
### Fixed
- Minor documentation fixes


## 0.1.0 - 2020-07-30
### Added
- Everything (`write`, `writeln`, `print`, `println`, `style`)


[Unreleased]: https://github.com/LukasKalbertodt/bunt/compare/v0.2.1...HEAD
[0.2.0]: https://github.com/LukasKalbertodt/bunt/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/LukasKalbertodt/bunt/compare/v0.1.1...v0.2.0
[0.1.1]: https://github.com/LukasKalbertodt/bunt/compare/v0.1.0...v0.1.1
