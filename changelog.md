# Changelog

A log of all notable changes to KMonad.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0)

## Unreleased

### Added

- Added names for the keys `rfkill` and `micmute` (#883).
  If you previously used the buttons `missing247` and `missing248`, please update to the new names.
- Added more MacOS keys (#936)
- Added keycodes above 255. If you are on linux you can use them now. (#935)
- Added `boot.initrd.services.kmonad.enable` NixOS option to use KMonad in the initrd (#941).
- Added `key-seq-delay`, a more general version of `cmp-seq-delay`, which enforces a minimum delay
  after each key event. (#908)
- Added `tap-hold-next-press` which is like `tap-next-press` but with an additional timeout. (#971)

### Changed

- `multi-tap` now holds when interrupted by another key while held. (#897)
- `key-seq-delay` defaults to 1ms (#975).
  If this slowdown in e.g. `tap-macro`s bothers you, you may want to set it to 0.
  If you used `cmp-seq-delay`, you probably no longer have to,
  as `key-seq-delay` effectivly already implies a `cmp-seq-delay`.

### Fixed

- Fixed `tapMacro` and `tapMacroRelease` behaviour which was slightly broken in #873 (#906)
- Fixed keycode translation problem on windows (#894)
- Fixed keyrepeat not working in tty on linux (#913)
- Fixed `multi-tap` not holding (#958)
- Fixed `multi-tap` cancelling on release of other keys (#974)

## 0.4.3 – 2024-09-11

### Added

- Added `stepped`. It performs the next button from a circular sequence
  whenever it is pressed.
- Implemented named source blocks.
  To name a source block add `:name <name>` at the beginning of the
  `defsrc` block. To use it add `:source <name>` after the layer name to the
  `deflayer` block. (#831)
- Added `around-only`, which works like `around` but releases the outer button
  when others are pressed. (#859)
- Added `around-when-alone`, which also represses the outer button when only
  the inner button is pressed and all others have been released. (#859)
- Allow customization of implicit `around`s (#859)

### Changed

- Update Karabiner-DriverKit to 3.1.0 (#780)
- Added tests to check that every button has documentation (#857)
- `defsrc` with duplicate keycodes are now forbidden (#860)

### Fixed

- Fixed crash on non-US backslash under MacOS (#766)
- Fixed broken keyboard due to circular event handling under MacOS (#781)
- Fixed crash on unhandled buttons by ignoring them (#807)
- Fixed parse errors relating to whitespace (#796, #875)
- Fixed broken compose sequences (#823, #869)
- Fixed parse errors when using keys only available on Darwin OS (#828)
- Fixed `around-next` wasn't parsable (#857)
- Fixed most buttons which behave weird in nested tap situations (#873)

## 0.4.2 – 2023-10-07

### Added

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
- Added `--version` (`-V`) flag
- Added `+,` for  "add a cedilla"
- Added `:timeout-button` keyword to `tap-hold-next` and
  `tap-hold-next-release`, so that they can switch to a button other than the
  hold button when the timeout expires.
- Added openrc startup script

### Changed

- Reorganized codebase
- The `multi-tap` key now immediately taps the current key when another
  key is pressed during tapping.

### Fixed

- Fixed compilation error under Mac, having to do with typo in Keycodes
- Fixed issue with empty-names for uinput-sinks
- Ignore SIGCHLD to deal with non-termination bug

## 0.4.1 - 2020-09-12

- First release where we start tracking changes.
