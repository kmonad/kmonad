# Changelog
A log of all notable changes to KMonad.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0)

## [Unreleased]

### [Added]
- Added `around-next-single`, a variant of `around-next` that will release its
  context on any change, as opposed to only on the release of the 'arounded'
  button.
- Added default compose sequence for Ü
- Added systemd user unit
- Added runit startup script
- Added short delay in startup
- Added macOS 11.0 support
- Added a `sticky-key`
- Expanded documentation

### [Changed]
- Reorganized codebase
- The `multi-tap` key now immediately taps the current key when another
  key is pressed during tapping.

### [Fixed]
- Fixed compilation error under Mac, having to do with typo in Keycodes
- Fixed issue with empty-names for uinput-sinks
- Ignore SIGCHLD to deal with non-termination bug

## [0.4.1] - 2020-09-12
- First release where we start tracking changes.
