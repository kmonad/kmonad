# Neo in KMonad

This folder contains these two *KMonad* configurations:

- `neo.kbd`: A first try of implementing the *Neo* keyboard layout.
- `vou.kbd`: My personal keyboard layout based on *Neo*, showing how to use some additional *KMonad* features.

## Why KMonad?

There are already many Neo/AdnW/KOY/Bone implementations/drivers available for Windows/MacOS/Linux. So why to use KMonad?

- KMonad adds useful features that you usually can have only in external keyboards with advanced firmware like *QMK*. I use *KMonad* because of the function `tap-hold-next-release`. So I have all modifiers on normal letter keys (only when holding them down while pressing and releasing(!) another key).
- With *KMonad* you can have a single layout configuration for Windows, Linux and MacOS (in case you switch between operation systems and use a personalized layout).


## Neo

`neo.kbd` implements Neo but it's neither complete nor completely correct. Consider it as a starting point, in case you want to use KMonad as your Neo keyboard driver.

This config contains empty layers 5 and 6 because greek characters can't be inserted as easy as typing them into the config file. You need to use compose sequences (defined in *WinCompose* or `~/.XCompose`). I have inserted these layers just for showing how to implement layers dependent on other layers (technically you switch to layer 2 or 3 when holding down Shift or Mod3 before you can reach layer 5). (There are layers 5 and 6 defined in a block comment but with all symbols in a wrong order, actually matching the VOU layout).

Level 2 is special: The left shift key is defined as a Neo level 2 modifier. So you get the right symbols when you type `lsft` + number but this key doesn't work for `shift` + `home` or `shift` + mouse click. In these cases you have to use the right shift key. But you'll get QWERTY's level 2 when using `rsft` + number. Both shift keys work for creating capital letters, though. (See Issue #111.)

You can switch temporarily to QWERTZ (and back) by pressing `CapsLock+F6` (not completely QWERTZ, `shift` + number gives you QWERTY symbols).

The caps lock functionality (holding down both shift keys) and level 4 lock are not yet implemented.

By the way, KMonad is designed to be based on US keyboard layouts. So `neo.kbd` changes the keyboard layout (`setxkbmap us`) on Linux. On Windows, you have to change your layout manually to `English/USA` in the System Settings. You can also use `English (International)`. Then you've got `ä`, `ö`, `ü` and `ß` available without compose sequences but there is an issue (#84).


## VOU

`vou.kbd` is my personal configuration that I use everyday (on Linux). I also use it on Windows (accepting some limitations).

It shows how to have modifiers in the home row (letter when tapped, modifier when held down).

I don't use Neo's layers 5 and 6 but I have defined a `special` layer instead. I use it for switching tabs (in almost every program), switching buffers in vim and for going back and forth in Firefox and fish.

This layout also has defined some aliases for umlauts (but not all activated). For example, you get `ö` when holding `o` for at least 300 ms. If you use `@oo` instead of `@ö`, you get `multi-tap` behaviour: Tapping `o` once = `o`, twice = `ö`, three times = `oo`.
