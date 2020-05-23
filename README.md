# KMonad

Welcome to KMonad, a keyboard remapping utility for Linux and Windows.

Check the doc directory for information on:
- [installation](doc/installation.md)
- [configuration syntax](doc/syntax.md)
- [possible issues](doc/issues.md)
- [Windows limitation](doc/why_windows_why.md)



<!-- ## installing -->

<!-- ### linux -->

<!-- #### compile your own -->




<!-- # kmonad -->

<!-- welcome to kmonad! if you are currently running linux and want to experiment -->
<!-- with this new way of defining keyboard layouts, either: -->
<!-- - head over to [releases](https://github.com/david-janssen/kmonad/releases) for -->
<!--   the latest static binary. -->
<!-- - consult the [installation section](readme.md#compiling) -->

<!-- we now have (limited) windows support! look [down below](readme.md#compiling) -->
<!-- for instructions on how to build your windows-version of kmonad, or check out -->
<!-- our [windows binary](https://github.com/david-janssen/kmonad/releases). check -->
<!-- out the [limitations on windows support](readme.md#windows-limitations) below. -->

<!-- ## what is kmonad? -->

<!-- kmonad is a keyboard remapping utility written to provide functionality that -->
<!-- aligns with that provided by the amazing [qmk -->
<!-- firmware](https://github.com/qmk/qmk_firmware/). the qmk firmware is compiled -->
<!-- and installed on programmable keyboards and allows your keyboards to transform -->
<!-- from comfortable clacky ("wahaay!" for mechanical switches) to amazingly useful, -->
<!-- by introducing the ability to overlap various maps of keys, have different -->
<!-- functionality for the same key when held or tapped, or create buttons that can -->
<!-- be tapped multiple times to have different effects. -->

<!-- however, we can't always have our programmable keyboard with us, and i -->
<!-- personally don't like using it with my laptop, because it either becomes a -->
<!-- balancing act, or i have to sit at a table with my laptop uncomfortably far away -->
<!-- from me. additionally, there are loads of people who do not have programmable -->
<!-- keyboards who might enjoy all the bells and whistles that qmk has to offer, and -->
<!-- so `kmonad` was born. -->


<!-- ## getting kmonad -->

<!-- ### binaries -->
<!-- kmonad is written in haskell (with a tiny bit of c). the lovely people over at -->
<!-- https://github.com/nh2/static-haskell-nix have helped me figure how to compile a -->
<!-- static binary that should work basically on any standard 64-bit linux -->
<!-- system. you can find the most recent release [on the releases -->
<!-- page](https://github.com/david-janssen/kmonad/releases). -->

<!-- ### compiling -->
<!-- #### linux -->
<!-- probably the easiest way to compile kmonad is using `stack`. if you do not have `stack` -->
<!-- installed, check https://docs.haskellstack.org/en/stable/readme/ for -->
<!-- instructions on installing it. after compilation, it can be removed again, since -->
<!-- `kmomad` does not need to be recompiled upon configuration. -->

<!-- after potentially installing `stack` and cloning this repo, you can build -->
<!-- `kmonad` by calling: -->
<!-- ``` shell -->
<!-- stack build -->
<!-- ``` -->

<!-- or call the following (currently broken, documentation will be fixed up in the future): -->
<!-- ``` shell -->
<!-- stack haddock --no-haddock-deps -->
<!-- ``` -->
<!-- to build a kmonad binary and the haddock documentation. i have put some effort -->
<!-- into documenting the code if you want to have a look around. it is nowhere near -->
<!-- perfect, and i hope to do more in the future. -->

<!-- `stack` will tell you where it saved the compiled binary after which you can -->
<!-- copy it to somewhere on your path. -->

<!-- #### windows -->
<!-- windows support was added under windows10 using a [haskell platform -->
<!-- installation](https://www.haskell.org/platform/). additionally, you might need -->
<!-- to install [mingw](http://mingw.org/) to provide `gcc` for windows to compile -->
<!-- the c-interface to windows. once both the haskell platform and mingw are -->
<!-- installed and available on the path, compiling kmonad should be identical to -->
<!-- linux, i.e.: -->

<!-- ``` powershell -->
<!-- stack build -->
<!-- ``` -->

<!-- ### packaged on various distros -->

<!-- #### on void linux -->
<!-- you can install `kmonad` via `xbps-install`: -->
<!-- ``` shell -->
<!-- xbps-install -s kmonad -->
<!-- ``` -->

<!-- #### guix -->
<!-- you can install `kmonad` via the `guix` package manager. you will need to copy -->
<!-- the udev rules into place manually. -->

<!-- ``` shell -->
<!-- guix install kmonad -->
<!-- sudo cp $(guix build kmonad)/lib/udev/rules.d/70-kmonad.rules /lib/udev/rules.d/ -->
<!-- ``` -->

<!-- if you use the guix system to manage your entire machine, you will instead want -->
<!-- to install udev rules using something like this in your `config.scm` -->

<!-- ``` scheme -->
<!-- (use-modules (gnu packages haskell-apps)) -->

<!-- (operating-system -->
<!--  ;; ... -->
<!--  (services -->
<!--   (modify-services %desktop-services -->
<!--     (udev-service-type config => -->
<!--       (udev-configuration (inherit config) -->
<!--        (rules (cons kmonad -->
<!--                     (udev-configuration-rules config)))))))) -->
<!-- ``` -->

<!-- ## running -->
<!-- kmonad currently requires 1, and exactly 1 input argument: a path to a -->
<!-- configuration file that describes the keyboard layout to run. for a guide to -->
<!-- writing valid configuration files, [see the -->
<!-- syntax -->
<!-- guide](https://github.com/david-janssen/kmonad/blob/master/doc/syntax_guide.md) -->
<!-- or [some of the examples](https://github.com/david-janssen/kmonad/tree/master/example). -->

<!-- once the compiled binary is on the path, running kmonad is as simple as: -->

<!-- ``` shell -->
<!-- kmonad /path/to/config/file.kbd -->
<!-- ``` -->

<!-- the method of running kmonad under windows is exactly the same: you use the -->
<!-- shell (for example: powershell) to start kmonad. for example, put the compiled -->
<!-- kmonad executable and config file in the same directory, start powershell, cd to -->
<!-- the directory, and run: -->

<!-- ``` powershell -->
<!-- ./kmonad config_file.kbd -->
<!-- ``` -->

<!-- this has the added benefit that, if kmonad experiences issues, you can use your -->
<!-- mouse to close the powershell and hopefully release the keyboard-hook. -->

<!-- note that this interface is extremely provisional and subject to change. -->

<!-- any kind of internal kmonad error that indicates that something has gone -->
<!-- seriously wrong with our representation of the computation will terminate kmonad -->
<!-- and display the error to stdout. it is however not uncommon for kmonad to have -->
<!-- to reacquire a uinput keyboard on resume from suspend. to that extent, any core -->
<!-- io exception will cause kmonad to pause for a second and attempt a restart, ad -->
<!-- infinitum. this means its fine to unplug the mapped keyboard and plug it back -->
<!-- in, without crashing kmonad.  -->

<!-- ## common issues -->

<!-- ### uinput permissions -->
<!-- currently, the only supported operating system is linux. kmonad uses the -->
<!-- `uinput` subsystem to write events to the operating system. if you want to be -->
<!-- able to run kmonad without using sudo (highly recommended to avoid sudo wherever -->
<!-- possible), you will need to ensure that your user is part of the `uinput` group. -->
<!-- on most linux's this can be achieved by: -->

<!-- ``` shell -->
<!-- sudo usermod -ag uinput username -->
<!-- ``` -->

<!-- if the `uinput` group does not exist, check whether your system has an `input` -->
<!-- group and try adding your user to that one instead. if this does not work, -->
<!-- create a new group with: -->

<!-- ``` shell -->
<!-- sudo groupadd uinput -->
<!-- ``` -->

<!-- you then have to add a udev rule (in either `/etc/dev/rules.d` or -->
<!-- `/lib/udev/rules.d`) with the following content: -->

<!-- ``` shell -->
<!-- kernel=="uinput", mode="0660", group="uinput", options+="static_node=uinput" -->
<!-- ``` -->

<!-- additionally, you might need to ensure that the `uinput` drivers are loaded -->
<!-- before starting kmonad, this can be achieved through: -->

<!-- ``` shell -->
<!-- sudo modprobe uinput -->
<!-- ``` -->

<!-- this might have to be repeated whenever you restart your computer. there are -->
<!-- various techniques for getting the `uinput` subsystem to load automatically, but -->
<!-- i didn't manage to get any of them to work. -->

<!-- ### figuring out which event-file corresponds to your keyboard  -->
<!-- sometimes you can find your keyboard listed under `/dev/input/by-id`. if so, -->
<!-- this is by far the best solution, since there is no guarantee that a keyboard -->
<!-- will be assigned the same numbered event-file. if this is not the case, however, -->
<!-- the easiest way to figure out which event-file corresponds to your keyboard is -->
<!-- probably to use the `evtest` linux utility.  -->

<!-- ### getting special characters to work -->
<!-- since kmonad only deals in 'raw', primitive keyboard events, there is no such -->
<!-- thing at that level as a special symbol. instead we emit common keyboard -->
<!-- sequences that the operating system needs to map to special characters. to that -->
<!-- extent, you need to indicate to x11 what key is supposed to trigger a -->
<!-- special-character macro. -->

<!-- there are two ways of doing this: -->
<!-- 1. manually, after launching kmonad, use either `xmodmap` or `setxkbmap` to -->
<!--    indicate to your os that 'right-alt' should be used as the compose key -->
<!--    (support for other compose keys is coming in the future). for example: -->

<!-- ``` shell -->
<!-- # either -->
<!-- xmodmap -e "keysym alt_r = multi_key"  -->
<!-- # or: -->
<!-- setxkbmap option compose:ralt -->
<!-- ``` -->

<!-- it is probably better to use `setxkbmap` here, since it resets your config -->
<!-- before applying modifications, whereas repeated calls to `xmodmap` can run into -->
<!-- errors because you are trying to map to buttons that have already been remapped. -->

<!-- 2. automatically, through the uinput_sink token. if you consult [the syntax -->
<!--    guide](doc/syntax_guide.md#output) you will see exactly how you can provide -->
<!--    kmonad with a shell-command to execute whenever a new uinput sink is created. -->
<!--    this has the added benefit that, whenever we need to recreate the uinput sink -->
<!--    (this is sometimes necessary after resuming from suspend, for example), the -->
<!--    command is automatically called again for you. -->
   
<!-- note that there is a small interval between creating a uinput sink and it -->
<!-- actually being registered by the os, so whether you manually call `setxkbmap` or -->
<!-- use the uinput_sink token to pass a shell command, you need to ensure that it -->
<!-- contains a small period of time for the os to register the keyboard. i have -->
<!-- found that 1 second is more than sufficient, but experiment yourself. -->

<!-- ### windows limitations -->
<!-- #### cannot distinguish between keyboards -->
<!-- the low-level api to the operating system differs significantly between windows -->
<!-- and linux, which means that the windows version is currently more limited in -->
<!-- what it can do. there is an active issue on this topic [over -->
<!-- here](https://github.com/david-janssen/kmonad/issues/10), and if you have -->
<!-- experience with `win32` programming, any help would be greatly appreciated. so -->
<!-- if you want to help, or just want a more technical overview of the windows -->
<!-- limitations head on over there. -->

<!-- currently, we cannot distinguish between different input keyboards, so whereas a -->
<!-- linux version of kmonad can be started for a variety of different keyboards, and -->
<!-- handle them all in different ways, the windows version of kmonad catches *all* -->
<!-- keyboard input signals. the only distinction kmonad makes under windows is -->
<!-- between 'real' keyboard events and simulated keyboard events. anything simulated -->
<!-- is automatically passed on to the os (that is also how kmonad avoids handling -->
<!-- its own simulated output).  -->

<!-- #### no native support for compose sequences -->
<!-- windows does not support the same compose-sequences as x11, meaning that the -->
<!-- special-character-emitting macros won't work out of the box. luckily there is -->
<!-- [wincompose](https://github.com/samhocevar/wincompose), a windows utility that -->
<!-- maps compose-key sequences to special characters. they say they have full -->
<!-- support for all x11 based compose-sequences, and in my limited test i did not -->
<!-- run into any problems. we currently do not support remapping the compose-key -->
<!-- internally, but the kmonad default lines up with the x11 default and the -->
<!-- wincompose default (right alt). -->

<!-- **note**: for wincompose to work with kmonad you *have* to enable support for -->
<!-- handling injected key events, which is *off* by default. -->

<!-- #### no idea how this interacts with ahk -->
<!-- since kmonad essentially grabs *all* standard keyboard input and lets through -->
<!-- only simulated events, there is no guarantee at all of this playing nice with -->
<!-- autohotkey at the moment. additionally, i am not entirely sure how windows deals -->
<!-- with its low-level keyboard hooks and how ahk tries to get at keyboard input, -->
<!-- but it might even be the case that different startup-orders could result in -->
<!-- different behavior. i have no experience with ahk at all, and rarely use -->
<!-- windows. if you run into any issues, please file them, and i'm sure that in time -->
<!-- we can resolve everything. -->

<!-- ### why can't i remap the fn key on my laptop? -->
<!-- many laptops have a fn key that mimics some of the functionality that kmonad -->
<!-- tries to offer: it changes the mapping of certain keys, like creating a numpad -->
<!-- in the middle of the laptop keyboard. this remapping happens in the hardware, -->
<!-- before any event is ever registered with the operating system, therefore kmonad -->
<!-- has no way to 'get' at any of those events. this means that we cannot remap them -->
<!-- in any way. -->
