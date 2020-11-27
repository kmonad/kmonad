## Linux

### Q: How do I get Uinput permissions?

A: In Linux KMonad needs to be able to access the uinput subsystem to inject
events. To do this, your user needs to have permissions. To achieve this, take
the following steps:

If the `uinput` group does not exist, check whether your system has an `input`
group and try adding your user to that one instead. If this does not work,
create a new group with:

1. Make sure the 'uinput' group exists

``` shell
sudo groupadd uinput
```

2. Add your user to the 'uinput' group:
``` shell
sudo usermod -aG uinput username
```

3. Make sure the uinput device file has the right permissions:
Add a udev rule (in either `/etc/dev/rules.d` or `/lib/udev/rules.d`) with the
following content:
```shell
KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
```

4. Make sure the `uinput` drivers are loaded.
You will probably have to run this command whenever you start kmonad for the first time.
``` shell
sudo modprobe uinput
```


### Q: How do I know which event-file corresponds to my keyboard?

A: By far the best solution is to use the keyboard devices listed under `/dev/input/by-id`. If you can't figure out which file just by the filenames, the `evtest` program is very helpful.


### Q: How do I emit Hyper_L?

A: `Hyper_L` is not a core Linux keycode, but is X11 specific instead. KMonad
tries to stay as close to the kernel as possible, so you can run it on other
OSes or without X11. If you want `Hyper_L` to work, you have to make sure that
X11 lines up well with KMonad. See [this issue](https://github.com/david-janssen/kmonad/issues/22) for more explanation.

### Q: How does Unicode entry work?

A: Unicode entry works via X11 compose-key sequences. For information on how to
configure kmonad to make use of this, please see [the tutorial](../keymap/tutorial.kbd).

## Windows

### How do I start KMonad?

A: This might be confusing if you are used to using a GUI and clicking on
things. Double clicking KMonad will look like it does nothing. KMonad is a
command-line utility, so, to run it you need to:

#### Using the command-line

1. Start a 'Command Prompt' (no need for 'as administrator')

2. 'cd' to where you've stored KMonad, like this:
```powershell
cd "C:\Users\david\Desktop\Just an Example"
```
NOTE: The double-tick marks around the path let you easily use directories with
spaces in the names.

3. Run the `kmonad` command (make sure the name matches exactly, so for the
   `0.4.0` version, that would be: `kmonad-0.4.0-windows.exe`, alternatively,
   rename the `kmonad` file to whatever you like and use that name). Depending
   on how you call it different things happen.

This will print the help and do nothing.
```powershell
kmonad.exe
```

This will start KMonad with the provided configuration file:
```powershell
kmonad.exe my_config.kbd
```

If the `my_config.cfg` file is not in the same directory as `kmonad`, you will
need to specify the full path to this file. (See [the
tutorial](/keymap/tutorial.kbd) for more information on how to write a
configuration.

```powershell
kmonad.exe C:\Users\david\Documents\my_config.kbd
```

You can even launch KMonad from anywhere (without having to do step 2. first) if
you use the full path for KMonad and the config file like this:
```powershell
"C:\Users\david\Desktop\Just an Example\kmonad.exe" C:\Users\david\Documents\my_config.kbd
```

If you want to really see what is happening on the inside of KMonad as it runs,
consider adding the `--log-level debug` flag like this:
```powershell
C:\pth\to\kmonad.exe some_config.kbd --log-level debug
```

This will cause KMonad to print out more information about what it is doing
moment to moment (without affecting anything else).

#### Making a launcher

If you want to start KMonad at the click of a button, consider making a shortcut
using the 'New' > 'Shortcut' entry on the right-click menu (if you right-click
the Desktop). Just select 'KMonad' and give it a name. Afterwards, right click
the shortcut and select 'Properties'. This should put you in the 'Shortcut' tab
of the properties, here there is a field called 'Target'. This field is exactly
like the shell command we used above, so copy-paste the exact command you used
to start KMonad into 'Target', then click apply, and you should now have a
clickable KMonad launcher.

## Mac

### Q: How to use the special features printed on Apple function keys?

A: By default, function keys on Apple keyboards trigger special features
(changing brightness, volume, etc.) when pressed alone, and act as traditional
function keys (F1, F2, etc.) when pressed with <kbd>fn</kbd>. Technically, when
<kbd>F1</kbd> (e.g.) is pressed on an Apple keyboard, it sends the keycode
corresponding to F1; macOS then translates this keycode to a special feature
(depending on whether <kbd>fn</kbd> was pressed) in the [keyboard
driver](https://github.com/pqrs-org/Karabiner-VirtualHIDDevice/issues/1). But
`kmonad` intercepts key presses before this translation can occur, and it emits
keypresses through a driver of its own. Therefore macOS does not translate any
keypresses emitted by kmonad, and the checkbox labeled "Use F1, F2, etc. keys as
standard function keys" in `System Preferences` will have no effect on keyboards
modified by kmonad.

However, we can simulate the default behavior of Apple keyboards by emitting
keycodes that correspond to the special features printed on the function
keys. See [keymap/template/apple.kbd](../keymap/template/apple.kbd) for an
example.

## General

### Q: Why doesn't the 'Print' keycode work for my print screen button?

A: Because the Keycode for "print screen" is actually 'SysReq' ("ssrq" or "sys")
for relatively interesting historical reasons. Have a look at [this
issue](https://github.com/david-janssen/kmonad/issues/59) if you want more
information.

### Q: Why can't I remap the Fn key on my laptop?

A: You cannot. Many laptops have a Fn key that mimics some of the functionality
that KMonad tries to offer: it changes the mapping of certain keys, like
creating a numpad in the middle of the laptop keyboard. This remapping happens
in the hardware, before any event is ever registered with the operating system,
therefore KMonad has no way to 'get' at any of those events. This means that we
cannot remap them in any way.
