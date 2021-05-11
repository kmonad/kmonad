# How KMonad is organized into modules

## Disclaimer
This is essentially a statement of intent. The next few steps in KMonad are
going to be a refactoring and reorganization of the code-base in preparation for
a redesign of the core model, so this functions as a sketch of how I intend to
organize the code. Once the reorganization is finished, this disclaimer can me
deleted, and hopefully the remaining documentation will accurately reflect how
the code is organized.

## Library vs. App
Nearly all the code in KMonad is written in a `kmonad` library, which lives in
in the `src` directory. The actual `kmonad` app, as defined by the
`kmonad.cabal` lives in the `app` directory, but is is solely an import of
`main` from `KMonad.App`.

## Main KMonad modules

KMonad is organized into 3 main modules, with additionally a custom Prelude.

### App
The `App` module is responsible for defining all the KMonad-specific data, like
what a `Keycode` or a `KeyEvent` is, and a variety of KMonad-specific IO tasks.

These all belong in App:
- logging
- keyboard io
- keyboard specific data declarations and operations
- invocation parsing
- configuration parsing
- the entrypoint of KMonad

Essentially, in App we have the `main` function. It then starts doing a variety
of IO tasks to set up an environment in which to run our model. This environment
is then handed off to `Model`, which actually does something interesting with
it.

NOTE: the `App` module contains a sub-module named `OS`, this submodule is *the
only place* where we write OS-specific code. If we start sprinkling `CPP`
through the rest of KMonad, things will become a mess. More about how
OS-specific code is organized below.

### Model
The `Model` module contains the logic of what we actually do. It defines things
like the different kinds of buttons we support, the fact that we organize
keymaps into layers, etc.

NOTE: `Model` is currently muddled. The future steps for KMonad involve first
reorganizing the code and dealing with keycode representations. When `App` is
solid, a `Model` reorganization is coming.

### Util
The `Util` module contains all the code that we want to use in KMonad, but that
isn't actually particularly keyboard specific. Here is where we put things that
we might one day just copy-paste into a completely different project. 

### Prelude
`Prelude` contains the code we assume to be present in every single other module
we write. We basically use `RIO`, but modify parts of the namespace to include
extremely commonly used functions. 

## Organization of a module
There is always a balancing act between module count and module size. This is
probably just a matter of taste, but I like to err on the side of small files.
To that extent, I've started dividing large files into submodules in a
directory, with a similarly named file that is responsible for importing the
directory.

When separating 1 file into a directory of smaller files, we generally follow
the convention of the following 3 files:
- Types.hs: may not import anything else from within this module, contains all
    the data, instance, class, and type declaration
- Ops.hs: for pure operations on the types
- IO.hs: for IO-operations involved with the types

For conceptually complex or large modules, feel free to decide on well-named and
well-organized module organizations. In general, ask yourself:
1. Will breaking convention make this clearer?
2. Will it really?
3. Really really?
If you answer yes to all 3 questions, go ahead.


## OS-specific code
Dealing with Linux/OSX/Windows has been a bit of a headache, and looks like it
could become an even greater headache if we don't handle this well. To that
extent we have organized the code in the following manner:

In `KMonad.App` there lives a module named `OS`

This module contains the following submodules:
- `Common`: for code shared between all 3
- `Linux`: for code imported only in Linux
- `Mac`: for code imported only in Mac
- `Windows`: for code imported only in Windows

Note, however, that configurations should parse *on any OS*. This means that the
configuration records for, for example, key-IO, should be defined on all
platforms. To that extent, the `Common` module itself contains 3 submodules:
- `Linux`: for code pertaining to Linux, but runable everywhere
- `Mac`: for code pertaining to Mac, but runable everywhere
- `Windows`: for code pertaining to Windows, but runable everywhere

This might seem circumspect, and it does mean that the configuration-record is
specified in a different module than the runtime-environment for various key-IO
tasks, but out of all the solutions I could think of this seemed the clearest.

The `KMonad.App.OS` module uses the `CPP` extension to change how things are
imported across different platforms.

This also means that when we run across something that should be slightly
different on different OSes, we might have to write a bit of plumbing code to
define these things in `KMonad.App.OS`, but that does mean that all the
OS-specific stuff is gathered in one place.

