# Installation

Jump to
- [Compilation](installation.md#compilation)
  - [Using nix](installation.md#using-nix)
  - [Using stack](installation.md#using-stack)
  - [Windows environment](installation.md#windows-environment)
  - [macOS](installation.md#macos)
- [Binaries](installation.md#binaries)
- [Packages](installation.md#packages)
  - [Void Linux](installation.md#void-linux)
  - [Guix](installation.md#guix)

## Compilation

### Using `stack`
The currently recommended, cross-platform solution to compiling KMonad is to use the
[stack](https://github.com/commercialhaskell/stack) Haskell project manager.

Once you have `stack` installed, you can build `kmonad` thusly:
```shell
stack build   # To build only the binary
stack haddock # To build the binary and the docs
```

If you would like `stack` to automatically copy the binary to a folder on your
`$PATH`, you can use:
```shell
stack install # Builds *and* copies
```

### Using `nix`
If you use the [Nix package manager](https://github.com/NixOS/nix), either
because you installed it yourself or because you are using NixOS, you can build
`kmonad` using the following command.

```shell
nix-build nix
```

Another option with `nix` is to use the `nix-shell` to ensure you have the
correct environment to run `stack` in. You can enter the development environment
using:

```shell
nix-shell nix/shell.nix
```


Note: we do also have to compile a little bit of C-code, so make sure `gcc` is
installed as well.

### Static compilation
Every now and then we compile and release a static binary for Linux that should
run on any Linux regardless of the installed libraries (i.e. `ldd` returns `not a
dynamic executable`). If, for some reason, you want to compile a static binary for the state of HEAD yourself, please copy the contents of `./nix/static` into the `kmonad` project root, and then call:

```shell
$(nix-build --no-link -A fullBuildScript)
```


### Windows environment

I have little experience with Haskell under windows, but I managed to compile
`kmonad` under Windows10 using a [Haskell platform
installation](https://www.haskell.org/platform). I also needed to install
[mingw](http://mingw.org) to provide `gcc`. With both the Haskell platform and
`mingw` building `kmonad` under Windows10 should as simple as `stack build`.

### macOS

kmonad supports macOS 10.12 to 10.15 (Sierra, High Sierra, Mojave, and
Catalina). Support for macOS 11.0 (Big Sur) is in progress.

Note: under macOS, `kmonad` uses a [kernel
extension](https://github.com/pqrs-org/Karabiner-VirtualHIDDevice)
(kext) to post modified key events to the OS. It is bundled with
kmonad as a submodule in `c_src/mac/Karabiner-VirtualHIDDevice`.

#### Installing the kext

You can either build the kext from source or you can install it as a
binary that is signed by its maintainer. Building from source is
difficult, and macOS won't load your kext unless you sign it with an
"Apple Developer ID," so we recommend installing the kext as a signed
binary.

The kext used by kmonad is maintained as part of
[Karabiner-Elements](https://github.com/pqrs-org/Karabiner-Elements).
Therefore, if you use Karabiner-Elements, you already have the kext
installed (though maybe a different version number). Run `kextstat |
grep Karabiner` to check the version: if
`org.pqrs.driver.Karabiner.VirtualHIDDevice.v061000 (6.10.0)` is
listed, then the installed kext is compatibile with kmonad and you can
move onto [installing kmonad](#installing-kmonad). If another version
is listed, this may work too (but has not been tested).

If you want to attempt building and signing the kext yourself, look to
[the
documentation](https://github.com/pqrs-org/Karabiner-VirtualHIDDevice)
for instructions. Otherwise, to install the kext as a signed binary, run:

```shell
git clone --recursive https://github.com/david-janssen/kmonad.git
cd c_src/mac/Karabiner-VirtualHIDDevice
make install
```

#### Installing kmonad

Compilation under Mac currently works with `stack`. Compilation under
Mac via `nix` is not tested or planned. Installation under Mac via
Hackage is not tested, but may work for the adveturous. To compile on
Mac, download the kmonad source:

```shell
git clone --recursive https://github.com/david-janssen/kmonad.git
```

Then build kmonad with `stack`:

```shell
stack build --extra-include-dirs=c_src/mac/Karabiner-VirtualHIDDevice/dist/include
```

## Binaries

You can download binaries for Windows and Linux (64bit) from the [releases page](https://github.com/david-janssen/kmonad/releases). Many thanks to [these lovely people](https://github.com/nh2/static-haskell-nix) for making this possible.

## Packages

Some people have gone out of their way to add `kmonad` into the package-managers of various distros. If you want to add `kmonad` to your distro and add a pull-request to update the documentation to reflect that, please feel free. 

NOTE: These packages might be out of date.

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

### NixOS

There is not currently a `kmonad` package in `nixpkgs`, however the following instructions show
how to create your own adhoc derivation, and how to configure udev rules in nixos.

#### The Derivation
Create a `kmonad.nix` derivation such as this one which fetches a static binary release of kmonad and packages it in the nix-store:
```
let
  pkgs = import <nixpkgs> { };

  kmonad-bin = pkgs.fetchurl {
    url = "https://github.com/david-janssen/kmonad/releases/download/0.3.0/kmonad-0.3.0-linux";
    sha256 = "4545b0823dfcffe0c4f0613916a6f38a0ccead0fb828c837de54971708bafc0b";
  };
in
pkgs.runCommand "kmonad" {}
    ''
      #!${pkgs.stdenv.shell}
      mkdir -p $out/bin
      cp ${kmonad-bin} $out/bin/kmonad
      chmod +x $out/bin/*
    ''
```

#### Configuration.nix

1. Import `kmonad.nix` into your `configuration.nix` file using a `let` expression:
```
let
  kmonad =  import /path/to/kmonad.nix;
in {
  <your_config>
}
```

2. Add `kmonad` to `environment.systemPackages`:
```
  environment.systemPackages = with pkgs; [
    ...
    kmonad
    ...
  ];
```

3. Create the `uinput` group and add your user to `uinput` and `input`:
```
  users.groups = { uinput = {}; };

  users.extraUsers.userName = {
    ...
    extraGroups = [ ... "input" "uinput" ];
  };
```

4. Add `udev` rules:
```
  services.udev.extraRules =
    ''
      # KMonad user access to /dev/uinput
      KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
    '';
```

5. Rebuild system:
```
sudo nixos-rebuild switch
```
