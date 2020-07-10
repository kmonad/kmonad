# KMonad

NOTE: The `refactor` branch is currently stable, and the suggested way to use `kmonad`. The only thing lacking is documentation. Once `refactor` is fully documented, `master` will be switched, but `refactor` should be considered as the recommended way to use `kmonad`.

Welcome to KMonad! If you are currently running Linux and want to experiment
with this new way of defining keyboard layouts, either:
- Head over to [releases](https://github.com/david-janssen/kmonad/releases) for
  the latest static binary.
- Consult the [installation section](README.md#Compiling)

We now have (limited) Windows support! Look [down below](README.md#Compiling)
for instructions on how to build your Windows-version of KMonad, or check out
our [Windows binary](https://github.com/david-janssen/kmonad/releases). Check
out the [limitations on windows support](README.md#windows-limitations) below.

## What is KMonad?

KMonad is a keyboard remapping utility written to provide functionality that
aligns with that provided by the amazing [QMK
firmware](https://github.com/qmk/qmk_firmware/). The QMK firmware is compiled
and installed on programmable keyboards and allows your keyboards to transform
from comfortable clacky ("Wahaay!" for mechanical switches) to amazingly useful,
by introducing the ability to overlap various maps of keys, have different
functionality for the same key when held or tapped, or create buttons that can
be tapped multiple times to have different effects.

However, we can't always have our programmable keyboard with us, and I
personally don't like using it with my laptop, because it either becomes a
balancing act, or I have to sit at a table with my laptop uncomfortably far away
from me. Additionally, there are loads of people who do not have programmable
keyboards who might enjoy all the bells and whistles that QMK has to offer, and
so `KMonad` was born.

### What about that name, KMonad?

The name KMonad is a wink at my favorite window manager:
[XMonad](https://github.com/xmonad/xmonad), but where XMonad manages your
X Window in its X-monad, we manage your Keys in our K-monad (actually, we call
it App, but who's checking).

### What can I do with KMonad?

Well, there are a variety of customizations you can make. For example, this
duplicates the functionality of another popular Linux utility: XCAPE, which
allows you to specify mod-buttons that also emit events when you simply tap
them.

For example, look at that useless CapsLock key on your keyboard. It is in a
**great** position, but do we ever use it? Well, what if you could remap
CapsLock to a button that behaves like a Ctrl key? That's already quite nice.
What if it could also emit 'Esc' when you tap it? Yep: all of that is possible
(Note: some of this is already possible through existing tools or udev rules).

Do you write a lot of Haskell code and define your own operators? Do you want
your operators to look like this: =>=->->>? That's a lot to type. Why not just
define a macro that does it for you?

The same technique can be used to create macros that insert a variety of
accented or umlauted characters, depending on your OS. Why type `Right-Alt " e`
when you can put this macro in a layer on a button close to home row?

Or more practically: do you really like the Dvorak layout, but do you also wish
you had very simple, left-handed access to the common Control z x c v shortcuts?
Have a look at [this dvorak
example](https://github.com/david-janssen/kmonad/blob/master/example/dvorak_sane_zxcv.kbd)
to see an easy solution.

For more ideas, check the `example` subdirectory, or the [syntax
guide](https://github.com/david-janssen/kmonad/blob/master/doc/syntax_guide.md)
for an overview of different button-types.

## Getting KMonad

### Binaries
KMonad is written in Haskell (with a tiny bit of C). The lovely people over at
https://github.com/nh2/static-haskell-nix have helped me figure how to compile a
static binary that should work basically on any standard 64-bit Linux
system. You can find the most recent release [on the releases
page](https://github.com/david-janssen/kmonad/releases).

### Compiling
#### Linux
Probably the easiest way to compile KMonad is using `stack`. If you do not have `stack`
installed, check https://docs.haskellstack.org/en/stable/README/ for
instructions on installing it. After compilation, it can be removed again, since
`kmomad` does not need to be recompiled upon configuration.

After potentially installing `stack` and cloning this repo, you can build
`kmonad` by calling:
``` shell
stack build
```

Or call the following (CURRENTLY BROKEN, documentation will be fixed up in the future):
``` shell
stack haddock --no-haddock-deps
```
to build a KMonad binary and the Haddock documentation. I have put some effort
into documenting the code if you want to have a look around. It is nowhere near
perfect, and I hope to do more in the future.

`stack` will tell you where it saved the compiled binary after which you can
copy it to somewhere on your path.

#### Windows
Windows support was added under Windows10 using a [Haskell Platform
installation](https://www.haskell.org/platform/). Additionally, you might need
to install [MinGW](http://mingw.org/) to provide `gcc` for windows to compile
the C-interface to Windows. Once both the Haskell Platform and MinGW are
installed and available on the path, compiling KMonad should be identical to
Linux, i.e.:

``` powershell
stack build
```

### Packaged on various distros

#### On Void Linux
You can install `kmonad` via `xbps-install`:
``` shell
xbps-install -S kmonad
```

#### Guix
You can install `kmonad` via the `guix` package manager. You will need to copy
the udev rules into place manually.

``` shell
guix install kmonad
sudo cp $(guix build kmonad)/lib/udev/rules.d/70-kmonad.rules /lib/udev/rules.d/
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

#### Nixos
There is not currently a `kmonad` package in `nixpkgs`, however the following instructions show
how to create your own adhoc derivation, and how to configure udev rules in nixos.

##### The Derivation
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

##### Configuration.nix

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


## Running
KMonad currently requires 1, and exactly 1 input argument: a path to a
configuration file that describes the keyboard layout to run. For a guide to
writing valid configuration files, [see the
syntax
guide](https://github.com/david-janssen/kmonad/blob/master/doc/syntax_guide.md)
or [some of the examples](https://github.com/david-janssen/kmonad/tree/master/example).

Once the compiled binary is on the PATH, running KMonad is as simple as:

``` shell
kmonad /path/to/config/file.kbd
```

The method of running KMonad under Windows is exactly the same: you use the
shell (for example: Powershell) to start KMonad. For example, put the compiled
KMonad executable and config file in the same directory, start Powershell, cd to
the directory, and run:

``` powershell
./kmonad config_file.kbd
```

This has the added benefit that, if KMonad experiences issues, you can use your
mouse to close the powershell and hopefully release the keyboard-hook.

Note that this interface is extremely provisional and subject to change.

Any kind of internal KMonad error that indicates that something has gone
seriously wrong with our representation of the computation will terminate KMonad
and display the error to stdout. It is however not uncommon for KMonad to have
to reacquire a uinput keyboard on resume from suspend. To that extent, any core
IO exception will cause KMonad to pause for a second and attempt a restart, ad
infinitum. This means its fine to unplug the mapped keyboard and plug it back
in, without crashing KMonad.

## Common issues

### Uinput permissions
Currently, the only supported operating system is Linux. KMonad uses the
`uinput` subsystem to write events to the operating system. If you want to be
able to run KMonad without using sudo (highly recommended to avoid sudo wherever
possible), you will need to ensure that your user is part of the `uinput` group.
On most Linux's this can be achieved by:

``` shell
sudo usermod -aG uinput username
```

If the `uinput` group does not exist, check whether your system has an `input`
group and try adding your user to that one instead. If this does not work,
create a new group with:

``` shell
sudo groupadd uinput
```

You then have to add a udev rule (in either `/etc/dev/rules.d` or
`/lib/udev/rules.d`) with the following content:

``` shell
KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
```

Additionally, you might need to ensure that the `uinput` drivers are loaded
before starting KMonad, this can be achieved through:

``` shell
sudo modprobe uinput
```

This might have to be repeated whenever you restart your computer. There are
various techniques for getting the `uinput` subsystem to load automatically, but
I didn't manage to get any of them to work.

### Figuring out which event-file corresponds to your keyboard
Sometimes you can find your keyboard listed under `/dev/input/by-id`. If so,
this is by far the best solution, since there is no guarantee that a keyboard
will be assigned the same numbered event-file. If this is not the case, however,
the easiest way to figure out which event-file corresponds to your keyboard is
probably to use the `evtest` Linux utility.

### Getting special characters to work
Since KMonad only deals in 'raw', primitive keyboard events, there is no such
thing at that level as a special symbol. Instead we emit common keyboard
sequences that the operating system needs to map to special characters. To that
extent, you need to indicate to X11 what key is supposed to trigger a
special-character macro.

There are two ways of doing this:
1. Manually, after launching KMonad, use either `xmodmap` or `setxkbmap` to
   indicate to your OS that 'Right-Alt' should be used as the compose key
   (support for other compose keys is coming in the future). For example:

``` shell
# Either
xmodmap -e "keysym Alt_R = Multi_key"
# or:
setxkbmap option compose:ralt
```

It is probably better to use `setxkbmap` here, since it resets your config
before applying modifications, whereas repeated calls to `xmodmap` can run into
errors because you are trying to map to buttons that have already been remapped.

2. Automatically, through the UINPUT_SINK token. If you consult [the syntax
   guide](doc/syntax_guide.md#Output) you will see exactly how you can provide
   KMonad with a shell-command to execute whenever a new uinput sink is created.
   This has the added benefit that, whenever we need to recreate the uinput sink
   (this is sometimes necessary after resuming from suspend, for example), the
   command is automatically called again for you.

Note that there is a small interval between creating a uinput sink and it
actually being registered by the OS, so whether you manually call `setxkbmap` or
use the UINPUT_SINK token to pass a shell command, you need to ensure that it
contains a small period of time for the OS to register the keyboard. I have
found that 1 second is more than sufficient, but experiment yourself.

### Windows Limitations
#### Cannot distinguish between keyboards
The low-level API to the operating system differs significantly between Windows
and Linux, which means that the Windows version is currently more limited in
what it can do. There is an active issue on this topic [over
here](https://github.com/david-janssen/kmonad/issues/10), and if you have
experience with `Win32` programming, any help would be greatly appreciated. So
if you want to help, or just want a more technical overview of the windows
limitations head on over there.

Currently, we cannot distinguish between different input keyboards, so whereas a
Linux version of KMonad can be started for a variety of different keyboards, and
handle them all in different ways, the Windows version of KMonad catches *all*
keyboard input signals. The only distinction KMonad makes under Windows is
between 'real' keyboard events and simulated keyboard events. Anything simulated
is automatically passed on to the OS (that is also how KMonad avoids handling
its own simulated output).

#### No native support for compose sequences
Windows does not support the same compose-sequences as X11, meaning that the
special-character-emitting macros won't work out of the box. Luckily there is
[WinCompose](https://github.com/SamHocevar/wincompose), a windows utility that
maps compose-key sequences to special characters. They say they have full
support for all X11 based compose-sequences, and in my limited test I did not
run into any problems. We currently do not support remapping the compose-key
internally, but the KMonad default lines up with the X11 default and the
WinCompose default (right Alt).

**NOTE**: For WinCompose to work with KMonad you *have* to enable support for
handling injected key events, which is *off* by default.

#### No idea how this interacts with AHK
Since KMonad essentially grabs *all* standard keyboard input and lets through
only simulated events, there is no guarantee at all of this playing nice with
AutoHotKey at the moment. Additionally, I am not entirely sure how Windows deals
with its low-level keyboard hooks and how AHK tries to get at keyboard input,
but it might even be the case that different startup-orders could result in
different behavior. I have no experience with AHK at all, and rarely use
Windows. If you run into any issues, please file them, and I'm sure that in time
we can resolve everything.

### Why can't I remap the Fn key on my laptop?
Many laptops have a Fn key that mimics some of the functionality that KMonad
tries to offer: it changes the mapping of certain keys, like creating a numpad
in the middle of the laptop keyboard. This remapping happens in the hardware,
before any event is ever registered with the operating system, therefore KMonad
has no way to 'get' at any of those events. This means that we cannot remap them
in any way.
