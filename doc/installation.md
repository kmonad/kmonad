# Installation

Jump to
- [Compilation](installation.md#compilation)
  - [Using nix](installation.md#using-nix)
  - [Using stack](installation.md#using-stack)
  - [Windows environment](installation.md#windows-environment)
  - [Mac](installation.md#mac)
- [Binaries](installation.md#binaries)
- [Packages](installation.md#packages)
  - [Void Linux](installation.md#void-linux)
  - [Guix](installation.md#guix)

## Compilation
### Using `nix`
If you use the [Nix package manager](https://github.com/NixOS/nix), either
because you installed it yourself or because you are using NixOS, you can build
`kmonad` using the following command.

```shell
nix-build nix/release.nix
```

Currently the process ends with an error, however only after the compilation and
documentation-generation have been completed. If you look through output of
`nix` you will see the paths it mentions and can find both the binary and the
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

### Mac

Mac support only for version 10.12 to 10.15. Support for Mac version
11.0 is planned.

Note: `kmonad` uses a [kernel
extension](https://github.com/pqrs-org/Karabiner-VirtualHIDDevice)
(kext) to post modified key events to the OS. This kext is tedious to
build from source, and the OS won't load it unless you sign it with an
"Apple Developer ID."  An easier way to install this kext is to
install
[Karabiner-Elements](https://github.com/pqrs-org/Karabiner-Elements),
which uses the same kext.

Compilation under Mac currently works with `stack`. Compilation under
Mac via `nix` is not tested or planned. To compile on Mac, download
the kmonad source:
```shell
git clone --recursive https://github.com/david-janssen/kmonad.git
```

Then, if you want to attempt building and signing the kext yourself,
look to [the
documentation](https://github.com/pqrs-org/Karabiner-VirtualHIDDevice)
for instructions. Otherwise, make sure you have
[Karabiner-Elements](https://github.com/pqrs-org/Karabiner-Elements)
installed.

Finally, build kmonad with `stack`:
```shell
stack build
```

## Binaries

You can download binaries for Windows and Linux (64bit) from the [releases page](https://github.com/david-janssen/kmonad/releases). Many thanks to [these lovely people](https://github.com/nh2/static-haskell-nix) for making this possible.

## Packages

Some people have gone out of their way to add `kmonad` into the package-managers of various distros. If you want to add `kmonad` to your distro and add a pull-request to update the documentation to reflect that, please feel free. 

#### Void Linux 
You can install `kmonad` via `xbps-install`: 
``` shell 
xbps-install -s kmonad 
``` 

#### guix 
You can install `kmonad` via the `guix` package manager. you will need to copy 
the udev rules into place manually. 

``` shell 
guix install kmonad 
sudo cp $(guix build kmonad)/lib/udev/rules.d/70-kmonad.rules /lib/udev/rules.d/ 
``` 

If you use the guix system to manage your entire machine, you will instead want 
to install udev rules using something like this in your `config.scm` 

``` scheme 
(use-modules (gnu packages haskell-apps)) 

(operating-system 
 ;; ... 
 (services 
  (modify-services %desktop-services 
    (udev-service-type config => 
      (udev-configuration (inherit config) 
       (rules (cons kmonad 
                    (udev-configuration-rules config)))))))) 
``` 
