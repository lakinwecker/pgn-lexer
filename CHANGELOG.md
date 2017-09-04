# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Fixed
- Fixed the null move parsing optimization I did late last night.
- Fixed the example program.
- Removed memmap as a depenency
- Made PGNTokenIterator.bytes private and added a ::new constructor

## [0.1.0]
### Added
- This CHANGELOG file.
- An initial API for iterating over tokens in a PGN file.
- PGN parsing based on nome
