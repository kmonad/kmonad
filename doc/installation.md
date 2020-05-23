# Installation

Jump to
- [Compilation](doc/installation.md#compilation)
  - [Using nix](doc/installation.md#using-nix)
  - [Using stack](doc/installation.md#using-stack)
  - [Windows environment](doc/installation.md#windows-environment)
- [Binaries](doc/installation.md#binaries)
- [Packages](doc/installation.md#packages)
  - [Void Linux](doc/installation.md#void-linux)
  - [Guix](doc/installation.md#guix)

## Compilation
### Using `nix`
If you the [Nix package manager](https://github.com/NixOS/nix), either because
you installed it yourself or because you are using NixOS, you can build `kmonad`
using the following command.

```shell
nix-build nix/release.nix
```

Currently the process ends with an error, however only after the compilation and
documentation-generation have been completed. If you look through output of
`nix` you will see the paths it mentions and find both the binary and the
documentation. Perhaps someone more well-versed with `nix` can help me fix this
issue.

Another option with `nix` is to use the `nix-shell` to ensure you have the
correct environment to run `stack` in. You can enter the development environment
using:

```shell
nix-shell nix/shell.nix
```

### Using `stack`
You can compile `kmonad` using the
[stack](https://github.com/commercialhaskell/stack) Haskell project manager.
Once you have `stack` installed, you can build `kmonad` thusly:
```shell
stack build   # To build only the binary
stack haddock # To build the binary and the docs
```

Note: we do also have to compile a little bit of C-code, so make sure `gcc` is
installed as well.

### Windows environment

FIXME: Windows support only for version 0.3.0 and lower. This will be fixed in
the future.

I have little experience with Haskell under windows, but I managed to compile
`kmonad` under Windows10 using a [Haskell platform
installation](https://www.haskell.org/platform). I also needed to install
[mingw](http://mingw.org) to provide `gcc`. With both the Haskell platform and
`mingw` building `kmonad` under Windows10 should as simple as `stack build`.