# Q: How do I get Uinput permissions?

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


# Q: How do I know which event-file corresponds to my keyboard?

A: By far the best solution is to use the keyboard devices listed under `/dev/input/by-id`. If you can't figure out which file just by the filenames, the `evtest` program is very helpful.


# Q: How do I emit Hyper_L?

A: `Hyper_L` is not a core Linux keycode, but is X11 specific instead. KMonad
tries to stay as close to the kernel as possible, so you can run it on other
OSes or without X11. If you want `Hyper_L` to work, you have to make sure that
X11 lines up well with KMonad. See [this issue](https://github.com/david-janssen/kmonad/issues/22) for more explanation.


# Q: Why can't I remap the Fn key on my laptop?

A: Many laptops have a Fn key that mimics some of the functionality that KMonad
tries to offer: it changes the mapping of certain keys, like creating a numpad
in the middle of the laptop keyboard. This remapping happens in the hardware,
before any event is ever registered with the operating system, therefore KMonad
has no way to 'get' at any of those events. This means that we cannot remap them
in any way.
