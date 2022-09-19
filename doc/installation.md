# Installation

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Installation](#installation)
    - [Compilation](#compilation)
        - [Using `stack`](#using-stack)
        - [Using `nix`](#using-nix)
        - [Static compilation](#static-compilation)
        - [Using Docker](#using-docker)
        - [Windows environment](#windows-environment)
        - [macOS](#macos)
            - [Installing the kext](#installing-the-kext)
            - [Installing the dext](#installing-the-dext)
            - [Installing kmonad](#installing-kmonad)
            - [Giving kmonad additional permissions](#giving-kmonad-additional-permissions)
    - [Binaries](#binaries)
    - [Packages](#packages)
        - [Arch Linux](#arch-linux)
        - [GNU Guix](#gnu-guix)
        - [Void Linux](#void-linux)
        - [NixOS](#nixos)
            - [The Derivation](#the-derivation)
            - [Configuration.nix](#configurationnix)
            - [The NixOS module](#the-nixos-module)

<!-- markdown-toc end -->

## Compilation

Note that, regardless of which compilation method you choose, `git`
needs to be in `$PATH` when compiling kmonad.  This is because we insert
the current commit into the output of `--version` at compile time.

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
run on any Linux regardless of the installed libraries (i.e. `ldd` returns `not
a dynamic executable`). If, for some reason, you want to compile a static
binary for the state of HEAD yourself, please copy the contents of
`./nix/static` into the `kmonad` project root, and then call:

```shell
$(nix-build --no-link -A fullBuildScript)
```

### Using Docker
If you have Docker installed, you can build `kmonad` from source without the
need to install anything else on your system, since the build container will
always have all the needed build tools and dependencies.

This is very convenient if no binaries are available and you want to try some
other branch, you don't want to install build tools or they're not available
for your OS, etc. You can even use the provided `Dockerfile` for development
testing. As of now, the built image is not meant to *run* `kmonad`, just to
build it.

Just do this from the `Dockerfile` directory:
``` shell
# Build the Docker image which will contain the binary.
docker build -t kmonad-builder .

# Spin up an ephemeral Docker container from the built image, to just copy the
# built binary to the host's current directory bind-mounted inside the
# container at /host/.
docker run --rm -it -v ${PWD}:/host/ kmonad-builder bash -c 'cp -vp /root/.local/bin/kmonad /host/'

# Clean up build image, since it is no longer needed.
docker rmi kmonad-builder
```
You will find a `kmonad` binary in your current directory.

As an added bonus, with recent Docker versions you can build images straight
from public repo URLs, whithout even needing to clone the repo.
Do this as the build step (the first one) in the previous instructions:
``` shell
docker build -t kmonad-builder github.com/kmonad/kmonad.git
```

### Windows environment

I have little experience with Haskell under windows, but I managed to compile
`kmonad` under Windows10 using a [Haskell platform
installation](https://www.haskell.org/platform). I also needed to install
[mingw](http://mingw.org) to provide `gcc`. With both the Haskell platform and
`mingw` building `kmonad` under Windows10 should as simple as `stack build`.

You also can install MSYS2, Haskell and stack via [scoop](https://scoop.sh/).
Simply run these commands in Windows PowerShell:

```posh
   # set required privileges to run scripts (required for scoop installer)
   Set-ExecutionPolicy RemoteSigned -scope CurrentUser

   # install scoop (no admin rights required)
   iwr -useb get.scoop.sh | iex

   # install stack
   scoop install stack

   # clone the KMonad repository (assuming you have `git` installed)
   cd $HOME\Downloads
   git clone https://github.com/kmonad/kmonad.git
   cd kmonad

   # compile KMonad (this will first download GHC and msys2, it takes a while)
   stack build

   # the new kmonad.exe will be in .\.stack-work\install\xxxxxxx\bin\
   
   # install kmonad.exe (copies kmonad.exe to %APPDATA%\local\bin\)
   stack install
   
   # run kmonad.exe
   kmonad.exe .\path\to\config.kbd
```

### macOS

kmonad supports macOS 10.12 to 10.15 (Sierra, High Sierra, Mojave, and
Catalina) and macOS 11.0 (Big Sur).

Note: under macOS, `kmonad` uses one of two "system extensions" to
post modified key events to the OS. For macOS Catalina and prior, we
use a [kernel
extension](https://github.com/pqrs-org/Karabiner-VirtualHIDDevice)
(kext), which is bundled with kmonad as a submodule in
`c_src/mac/Karabiner-VirtualHIDDevice`. For macOS Catalina and later,
we use a [driverkit-based
extension](https://github.com/pqrs-org/Karabiner-DriverKit-VirtualHIDDevice)
(dext), bundled as
`c_src/mac/Karabiner-DriverKit-VirtualHIDDevice`. Therefore, you must
install either the kext or dext based on your macOS version (Catalina
users can choose either one).

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

``` console
  $ git clone --recursive https://github.com/kmonad/kmonad.git
  $ cd c_src/mac/Karabiner-VirtualHIDDevice
  $ make install
```

#### Installing the dext

You can either build the dext from source or you can install it as a
binary that is signed by its maintainer. Building from source is only
possible with an "Apple Developer ID," unless you build an old version
of the dext.

The dext used by kmonad is maintained as part of
[Karabiner-Elements](https://github.com/pqrs-org/Karabiner-Elements).
Therefore, if you use Karabiner-Elements, you may already have the
dext installed (though maybe a different version number). Run
`defaults read
/Applications/.Karabiner-VirtualHIDDevice-Manager.app/Contents/Info.plist
CFBundleVersion` to check the version: if `1.15.0` is shown, then the
installed dext is compatibile with kmonad and you can move onto
[installing kmonad](#installing-kmonad). If another version is listed,
this may work too (but has not been tested).

##### Build and sign the dext yourself

If you want to attempt building and signing the dext yourself, look to
[the
documentation](https://github.com/pqrs-org/Karabiner-DriverKit-VirtualHIDDevice)
for instructions.

##### Install the already build and signed dext package

To install the dext as a signed binary, initialize the dext git submodule,
install the extension, and activate the extension.

```console
  $ git clone --recursive https://github.com/kmonad/kmonad.git
```

```console
  $ cd kmonad/
  $ open c_src/mac/Karabiner-DriverKit-VirtualHIDDevice/dist/Karabiner-DriverKit-VirtualHIDDevice-1.15.0.pkg
```

```console
  $ /Applications/.Karabiner-VirtualHIDDevice-Manager.app/Contents/MacOS/Karabiner-VirtualHIDDevice-Manager activate
```

Note: If activation failed (e.g. because a newer version is already installed), replace `activate` in the above command with `forceActivate` and try again.


#### Installing kmonad

Compilation under Mac currently works with `stack`. Compilation under
Mac via `nix` is not tested or planned. Installation under Mac via
Hackage is not tested, but may work for the adveturous. To compile on
Mac, download the kmonad source:

``` console
  $ git clone --recursive https://github.com/kmonad/kmonad.git
```

Then build kmonad with `stack`. If you are building against the kext, run:

``` console
  $ stack build --flag kmonad:kext --extra-include-dirs=c_src/mac/Karabiner-VirtualHIDDevice/dist/include
```

If you are building against the dext, run

``` console
  $ stack build --flag kmonad:dext --extra-include-dirs=c_src/mac/Karabiner-DriverKit-VirtualHIDDevice/include/pqrs/karabiner/driverkit:c_src/mac/Karabiner-DriverKit-VirtualHIDDevice/src/Client/vendor/include
```

#### Giving kmonad additional permissions

Since Mac OS X Leopard (10.5), intercepting key events
[requires](https://developer.apple.com/library/archive/technotes/tn2187/_index.html#//apple_ref/doc/uid/DTS10004224-CH1-DontLinkElementID_10)
root privilege. Therefore, you must run kmonad as root (using `sudo`,
e.g.). In the future, privilege separation may be implemented so that
just a small part of kmonad requires root privilege to run.

Since macOS Catalina (10.15), capturing key events requires explicit
permission in `System Preferences`. Enable the application(s) that you
will be using to execute kmonad in `System Preferences` > `Security &
Privacy` > `Privacy` > `Input Monitoring`.

## Binaries

You can download binaries for Windows and Linux (64bit) from the
[releases page](https://github.com/kmonad/kmonad/releases). Many thanks
to [these lovely people](https://github.com/nh2/static-haskell-nix) for
making this possible.

## Packages

Some people have gone out of their way to add `kmonad` into the
package-managers of various distros. If you want to add `kmonad` to your
distro and add a pull-request to update the documentation to reflect
that, please feel free.

NOTE: These packages might be out of date.

### Arch Linux

Kmonad is available in the Arch User Repository (AUR) as
[kmonad-bin](https://aur.archlinux.org/packages/kmonad-bin).

### GNU Guix

You can install `kmonad` via the `guix` package manager. You will need to copy
the udev rules into place manually.

``` console
  $ guix install kmonad
  # cp <kmonad-path>/lib/udev/rules.d/70-kmonad.rules /lib/udev/rules.d/
```

According to Guix's package store mechanism, `<kmonad-path>` will include a hash
that captures the exact KMonad version. By default, the path will follow the
pattern `/gnu/store/<hash>-kmonad-<version>/`.

Use the `guix build kmonad` command to identify the correct path. In case the
command returns multiple paths, go for the shortest one.

So, for instance, if `build` returns

``` console
  $ guix build kmonad
  /gnu/store/9mx79afpjqxjiiqgh1xv3b7ckblnl4wk-kmonad-0.4.1
  /gnu/store/al0bmdxvl3a8s11vxn13y2nkq4hbg4c8-kmonad-0.4.1-static
```

`<kmonad-path>` will be
`/gnu/store/9mx79afpjqxjiiqgh1xv3b7ckblnl4wk-kmonad-0.4.1` and the copy
operation will then be as follows

``` console
  # cp \
  /gnu/store/9mx79afpjqxjiiqgh1xv3b7ckblnl4wk-kmonad-0.4.1/lib/udev/rules.d/70-kmonad.rules \
  /lib/udev/rules.d/
```

If you use the Guix System to manage your entire machine, you will instead want
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

### Void Linux

You can install `kmonad` via `xbps-install`:

``` console
  # xbps-install -S kmonad
```

### NixOS

There is not currently a `kmonad` package in `nixpkgs`, however the
following instructions show how to create your own adhoc derivation, and
how to configure udev rules in nixos. There is also a NixOS module
included in this repository that can be used instead of a manual
configuration.

#### The Derivation
Create a `kmonad.nix` derivation such as this one which fetches a static
binary release of kmonad and packages it in the nix-store:

``` nix
  let
    pkgs = import <nixpkgs> { };

    kmonad-bin = pkgs.fetchurl {
      url = "https://github.com/kmonad/kmonad/releases/download/0.3.0/kmonad-0.3.0-linux";
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

1. Import `kmonad.nix` into your `configuration.nix` file using a `let`
   expression:

   ``` nix
     let
       kmonad =  import /path/to/kmonad.nix;
     in {
       <your_config>
     }
   ```

2. Add `kmonad` to `environment.systemPackages`:

   ``` nix
     environment.systemPackages = with pkgs; [
       ...
       kmonad
       ...
     ];
   ```

3. Create the `uinput` group and add your user to `uinput` and `input`:

   ``` nix
     users.groups = { uinput = {}; };

     users.extraUsers.userName = {
       ...
       extraGroups = [ ... "input" "uinput" ];
     };
   ```

4. Add `udev` rules:

   ``` nix
     services.udev.extraRules =
       ''
         # KMonad user access to /dev/uinput
         KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
       '';
   ```

5. Rebuild system:

   ``` console
     # nixos-rebuild switch
   ```

#### The NixOS module

1. Clone this repository or copy the file `nix/nixos-module.nix`
   somewhere to your system.

2. Add `nixos-module.nix` into your `configuration.nix` as an import:

   ``` nix
     imports =
       [
         /path/to/nixos-module.nix;
       ];
   ```

3. Configure the module:

   ``` nix
     services.kmonad = {
       enable = true; # disable to not run kmonad at startup
       configfiles = [ /path/to/config.kbd ];
   	# Modify the following line if you copied nixos-module.nix elsewhere or if you want to use the derivation described above
   	# package = import /pack/to/kmonad.nix;
     };
   ```

4. If you've set `enable = true;` at the previous step, do not put a
   `setxkbmap` line in your `config.kbd`. Instead, set the options like
   this:

   ``` nix
     services.xserver = {
       xkbOptions = "compose:ralt";
       layout = "us";
     };
   ```

5. If you want your main user to use kmonad, add it to the `uinput` and
   `input` groups:

   ``` nix
     users.extraUsers.userName = {
       ...
       extraGroups = [ ... "input" "uinput" ];
     };
   ```

6. Rebuild system:

   ``` console
     # nixos-rebuild switch
   ```
