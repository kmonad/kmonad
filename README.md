# KMonad

If you have somehow stumbled into my little remote corner of GitHub and
encountered this project: welcome! Feel free to try KMonad, it is currently
extremely experimental. I will be continuing to test and document it over the
coming weeks, and when I am content it is ready for more widespread testing, I
will announce it in a number of places.

# Installing KMonad

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
