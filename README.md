# KMonad

If you have somehow stumbled into my little remote corner of GitHub and
encountered this project: welcome! Feel free to try KMonad, it is currently
extremely experimental. I will be continuing to test and document it over the
coming weeks, and when I am content it is ready for more widespread testing, I
will announce it in a number of places.

# Installing KMonad

You can either compile KMonad yourself using haskell. It should be very
straighforward with stack. Alternatively, once I decide that we're ready to
start testing at a larger scale, I will provide a release with a statically
compiled binary, thanks to the lovely people over at
https://github.com/nh2/static-haskell-nix. 

## Compiling
Currently KMonad is configured to be compiled by stack. If you do not have stack
installed, check https://docs.haskellstack.org/en/stable/README/ for
instructions on installing it. After compilation, it can be removed again, since
`kmomad` does not need to be recompiled upon configuration.

After potentially installing `stack` and cloning this repo, you can build
`kmonad` by calling:
``` shell
stack build
```

Or call the following:
``` shell
stack haddock --no-haddock-deps
```
to build a KMonad binary and the haddock documentation.

`stack` will tell you where it saved the compiled binary after which you can
copy it to somewhere on your path.

## Running
KMonad currently requires 1, and exactly 1 input argument: a path to a
configuration file that describes the keyboard layout to run. For a guide to
writing valid configuration files, [see the
syntax guide](https://github.com/david-janssen/kmonad/blob/master/doc/syntax_guide.md).

Once the compiled binary is on the PATH, running KMonad is as simple as:

``` shell
kmonad /path/to/config/file.kbd
```

Note that this interface is extremely provisional and subject to change.

Any kind of internal KMonad error that indicates that something has gone
seriously wrong with our representation of the computation will terminate KMonad
and display the error to stdout. It is however not uncommon for KMonad to have
to reacquire a uinput keyboard on resume from suspend. To that extent, any core
IO exception will cause KMonad to pause for a second and attempt a restart, ad
infinitum. This means its fine to unplug the mapped keyboard and plug it back
in, without crashing KMonad. 

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
