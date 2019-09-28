# KMonad

If you have somehow stumbled into my little remote corner of GitHub and
encountered this project: welcome! Feel free to try KMonad, it is currently
extremely experimental. I will be continuing to test and document it over the
coming weeks, and when I am content it is ready for more widespread testing, I
will announce it in a number of places.

# Installing KMonad

## Compiling
Currently KMonad is configured to be compiled by stack. After cloning the repo,
call:
``` shell
stack build
```
to build a KMonad binary. Or call
``` shell
stack haddock --no-haddock-deps
```
to build a KMonad binary and the haddock documentation.

## Running
KMonad currently requires 1, and exactly 1 input argument: a path to a
configuration file that describes the keyboard layout to run. For a guide to
writing valid configuration files, [see the
Wiki](https://github.com/david-janssen/kmonad/wiki/Configuration-Syntax)

Once the compiled binary is on the PATH, running KMonad is as simple as:

``` shell
kmonad /path/to/config/file.kbd
```

Note that this interface is extremely provisional and subject to change.

### Uinput permissions
Currently, the only supported operating system is Linux. KMonad uses the
`uinput` subsystem to write events to the operating system. If you want to be
able to run KMonad without using sudo (highly recommended to avoid sudo wherever
possible), you will need to ensure that your user is part of the `uinput` group.
On most linux's this can be achieved by:

``` shell
sudo usermod -aG uinput username
```

Additionally, you might need to ensure that the `uinput` drivers are loaded
before starting KMonad, this can be achieved through:

``` shell
sudo modprobe uinput
```

This might have to be repeated whenever you restart your computer. There are
various techniques for getting the `uinput` subsystem to load automatically, but
I didn't manage to get any of them to work.

# Known issues:
- Trying to emit a CapsLock crashes KMonad
