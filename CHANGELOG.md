# Changelog

All notable changes to this project will be documented in this file.


## [Unreleased]
### Changed
- Use crate `litrs` for string literal parsing instead of having custom code for that.
  This should get rid of some parsing errors for some edge cases.
  It also makes maintenance easier, as it removes quite a bit of code from `bunt`.

## [0.2.4] - 2020-11-19
### Added
- Add `dimmed` attribute, now requiring `termcolor = "1.1.1"` ([#19](https://github.com/LukasKalbertodt/bunt/pull/19))

## [0.2.3] - 2020-10-03
### Added
- Add way to pass multiple format strings (`println!(["abc", "bar"])` to work around the `concat!` limitation ([#17](https://github.com/LukasKalbertodt/bunt/pull/17))

### Changed
- Clarify that `bunt-macros` is treated as internal code and that you must not depend on it directly. That crate does *not* follow semantic versioning.

## [0.2.2] - 2020-09-26
### Fixed
- Make `?` work inside arguments (e.g. `println!("{}", foo?)`) ([#16](https://github.com/LukasKalbertodt/bunt/pull/16))

## [0.2.1] - 2020-09-20
### Added
- Add `eprint!` and `eprintln!` ([#13](https://github.com/LukasKalbertodt/bunt/pull/13))

## [0.2.0] - 2020-09-13
### Breaking changes
- Minimal Rust version bumped to 1.46.0

### Changed
- `syn` dependency removed ([#8](https://github.com/LukasKalbertodt/bunt/pull/8))
- Emit error if arguments are not used

### Fixed
- Implement width and precision non-constant arguments (e.g. `{:0$}` or
  `{:.prec$}` or `{:.*}`) ([#10](https://github.com/LukasKalbertodt/bunt/pull/10))
- Fix named arguments also working as positional ones
- Fix bug in parsing 0 flag in format spec (`{:0$}` now parses as "the width is
  specified in the first argument" instead of the zero flag)


## [0.1.1] - 2020-09-05
### Fixed
- Minor documentation fixes


## 0.1.0 - 2020-07-30
### Added
- Everything (`write`, `writeln`, `print`, `println`, `style`)


[Unreleased]: https://github.com/LukasKalbertodt/bunt/compare/v0.2.4...HEAD
[0.2.4]: https://github.com/LukasKalbertodt/bunt/compare/v0.2.3...v0.2.4
[0.2.3]: https://github.com/LukasKalbertodt/bunt/compare/v0.2.2...v0.2.3
[0.2.2]: https://github.com/LukasKalbertodt/bunt/compare/v0.2.1...v0.2.2
[0.2.1]: https://github.com/LukasKalbertodt/bunt/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/LukasKalbertodt/bunt/compare/v0.1.1...v0.2.0
[0.1.1]: https://github.com/LukasKalbertodt/bunt/compare/v0.1.0...v0.1.1
