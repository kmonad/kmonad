<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Introduction](#introduction)
- [Defcfg](#defcfg)
    - [Input and Output](#input-and-output)
    - [Other Configuration Options](#other-configuration-options)
    - [Full Example](#full-example)
- [Buttons](#buttons)
    - [Modded Buttons](#modded-buttons)
    - [General Purpose Buttons](#general-purpose-buttons)
    - [Tap buttons](#tap-buttons)
- [Layers](#layers)
    - [Operating on Layers](#operating-on-layers)

<!-- markdown-toc end -->

# Introduction
Welcome to the KMonad quick reference guide. This document aims to provide a
simple definition and a usable example for all possible configuration options in
the KMonad program to act as a quick reference that you look into when forgetting
a function or/and wanting to know something quickly. Note that this document
does not try to be a full guide or a detailed documentation, this is the aim of
[tutorial.kbd](../keymap/tutorial.kbd).

# Defcfg

The `defcfg` block is where you can define all of your configuration
rules like setting up in- and output devices or certain global
configuration options.

A block can take the following arguments:

## Input and Output

+ `input`: define the input keyboard which the program will capture.

+ `output`: define the output keyboard which kmonad will create, with
  additional options to execute upon starting kmonad.

Here is how you would define the basic input and output settings on all
supported systems:

  - GNU/Linux:

    ```clojure
    input  (device-file "/dev/input/by-id/usb-04d9_daskeyboard-event-kbd")
    output (uinput-sink "My KMonad output")
    ```

  - Windows:

    ```clojure
    input  (low-level-hook)
    output (send-event-sink)
    ```

  - MacOS:

    ```clojure
    input  (iokit-name "my-keyboard-product-string")
    output (kext)
    ```

## Other Configuration Options

The following are all global config options that one can set in the
`defcfg` block.

+ `fallthrough` (boolean, defaults to `false`): re-emit keys that are
  not defined in a `defsrc` block.

  This allows one to only specify certain parts of a layout, with all
  other keys having their "default" meaning.

+ `allow-cmd` (boolean, defaults to `false`): allow arbitrary shell
  commands to be run as keys.

  NOTE: This can be dangerous since someone with access to your keyboard
  file could bind `rm -rf ~/` to a key. Think about whether you really
  want this behaviour.

+ `cmp-seq` (key, defaults to `RightAlt`): compose key for Unicode input
  (X11 specific).

+ `cmp-seq-delay` (natural number): delay between each pressed key in a
  compose-key sequence.

## Full Example

Below is an example of a full `defcfg` block for a GNU/Linux system.

```clojure
(defcfg
  input  (device-file "/dev/input/by-id/usb-04d9_daskeyboard-event-kbd")
  output (uinput-sink
          "My KMonad output"                           ;; name of the created keyboard
          "sleep 1 && setxkbmap -option compose:ralt") ;; additional, environment-specific, information
  cmp-seq ralt
  cmp-seq-delay 5
  fallthrough true
  allow-cmd true
)
```

# Buttons

Defining fancy buttons is why we're here, right? There are a variety of
these things here, as well as some helpers to make entering them into
layers (more below) much easier.

## Press- or release-only buttons

- `(press-only x)` : Send the *press* of x when this button is tapped
- `(release-only x)` : Send the *release* of x when this button is tapped

It is possible to define buttons that only press or release a virtual output
button. They are useful in tap-macros, especially ones that are going to be
executed in a 'known context'. Assume you want to use your physical alt button
to *both* open a layer, but also to function as a basic `alt` key. This can be
achieved by `(around met (layer-toggle my-layer))`. However, if you have a macro
inside `my-layer` that taps alt, then this would release alt until the layer is
reactivated (by physically releasing and repressing the `alt` key). The macro,
however, can instead be affixed by `(press-only met)`, making the last step of
the macro the reactivation of the `alt` key, solving the problem.

## Modded Buttons

To make key-entry easier, kmonad already provides some syntax for
Emacs-like specification of key chords. They are defined like this:

  - `C-` : `(around lctl X)`
  - `A-` : `(around lalt X)`
  - `M-` : `(around lmet X)`
  - `S-` : `(around lsft X)`

Then `RC-`, `RA-`, `RM-`, and `RS-` behave exactly the same, except
using the right-modifier.

The definition of a key chord then looks like this:

```clojure
(defalias Ca C-a) ;; this is equivalent to (defalias Ca (around Ctl a))
```

## General Purpose Buttons

+ `defalias`: define a name for a button. This can then be referenced in
  a layer using the `@name` syntax (see [layers][] below).

  ```clojure
  (defalias
    num (layer-toggle numbers) ;; Bind num to a button that switches to a layer
    kil C-A-del                ;; Bind kil to a button that Ctrl+Alt+deletes
  )
  ```

+ `around`: combine two keys into one.

  ```clojure
  (defalias ad (around alt del)) ;; this is like pressing Alt+Del
  ```

+ `around-next`: perform the next button-press inside some context (like
  `layer-next` but more generalized)

  ```clojure
  (defalias ns  (around-next sft))  ;; Shift the next press
  ```
+ `around-next-timeout`: like `around-next` except that if other button press is not detected within
  some timeout, some other button is tapped.

  ```clojure
  (around-next-timeout 500 sft XX)
  ```
  
+ `sticky keys`: act like the key is held temporarily after just one
  press for the given amount of time (in ms).

  ```clojure
  (defalias slc (sticky-key 500 lctl))
  ```
  
+ `stepped button`: perform different buttons in sequence.
  ```clojure
  (stepped (press-only lctl) (release-only lctl))
  ```
  
  This button:
  - presses control the first time it is tapped
  - releases control the second time it is tapped
  

+ `pause`: pause for the given number of ms.

  ```clojure
  (defalias
    ta3 (tap-macro K P5 M P4 o P3 n P6 a (pause 5) d) ;; P5 = (pause 5)
  )
  ```

+ `cmd-button`: take two arguments, the second one of which is
  optional. These represent the commands to be executed on pressing and
  releasing the button respectively

  ```clojure
  (defalias
    dat (cmd-button "date >> /tmp/kmonad_example.txt")   ;; Append date to tmpfile
    pth (cmd-button "echo $PATH > /tmp/kmonad_path.txt") ;; Write out PATH
    ;; `dat' on press and `pth' on release
    bth (cmd-button "date >> /tmp/kmonad_example.txt"
                    "echo $PATH > /tmp/kmonad_path.txt")
  )
  ```

## Tap buttons

Tap buttons are an integral part of kmonad. Everyone has different
preferences—that's why there are so many! Particularly when using
home-row modifiers, you will find some of the more crazy seeming buttons
to be the most comfortable.

+ `tap-macro`: take a sequence of keys and tap them, but don’t release
  the last key until the button is released.

  ```clojure
  (defalias ta1 (tap-macro K M o n a d))
  ```

  A `tap-macro` can take an optional `:delay` keyword (in ms)—this will
  wait for that amount of time after each keypress:

  ```clojure
  (defalias
    ta1 (tap-macro K M o n a d :delay 5)
    ;; equivalent to: (tap-macro K P5 M P5 o P5 n P5 a P5 d)
  )
  ```

+ `tap-macro-release`: like `tap-macro` but don’t press the last key
  until the button is released.

  ```clojure
  (defalias ta2 (tap-macro-release K M o n a d))
  ```

+ `multi-tap`: combine a sequence of keys into one key with a timeout
  between each key, as well as a last (default) button without a delay.

  ```clojure
  (defalias mt  (multi-tap 300 a 300 b 300 c 300 d e))
  ```

+ `tap-next`: combine 2 buttons, one for when the button is tapped and
  one for when it is held. The decision of what to execute is based upon
  whether the next button is the buttons own release or not.

  ```clojure
  (defalias tan (tap-next a sft))
  ```

+ `tap-hold`: like `tap-next` but with a timeout for tapping. If the key
  is released before the timeout it taps, if not then it holds.

  ```clojure
  (defalias tah (tap-hold 200 a sft))
  ```

+ `tap-hold-next`: a combination of `tap-next` and `tap-hold`: like
  `tap-next`, but _also_ switch to hold after a period of time.

  ```clojure
  (defalias thn (tap-hold-next 1000 a sft))
  ```

+ `tap-next-release`: like `tap-next` but decide whether to tap or hold
  based on the next release of a key (that was not already pressed when
  this button was pressed).

  ```clojure
  (defalias tnr (tap-next-release a sft))
  ```

+ `tap-next-press`: like `tap-next` but decide whether to tap or hold
  based on whether another key is pressed before this one is released.

  ```clojure
  (defalias tnp (tap-next-press a sft))
  ```

+ `tap-hold-next-release`: like `tap-next-release` but with an
  additional timeout. This is just like `tap-next-release`, but with
  `tap-next` swapped out for `tap-next-release`.

  ```clojure
  (defalias thr (tap-hold-next-release 1000 a sft))
  ```
  
+ `tap-hold-next` and `tap-hold-next-release` can take an optional 
  `:timeout-button` keyword to specify a button other than the
  hold button which should be held when the timeout expires.

  ```clojure
  (defalias
    thn (tap-hold-next 200 a lsft :timeout-button x)
    thr (tap-hold-next-release 200 a lsft :timeout-button x)
  )
  ```

# Layers

KMonad allows you to define and operate on several _layers_; these are
just collections of keys.

+ `defsrc`: define the input keys of your physical keyboard. This should
  reflect the keys on your keyboard as closely as possible. Although a
  `defsrc` is very similar to a layer visually, it is not a one and will
  thus not be used as one! It only serves to define where the different
  keys are and what kind of layout kmonad is initially dealing with.

  For example, an ANSI 60% keyboard may be represented as:

  ```
  (defsrc
    grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
    tab  q    w    e    r    t    y    u    i    o    p    [    ]    \
    caps a    s    d    f    g    h    j    k    l    ;    '    ret
    lsft z    x    c    v    b    n    m    ,    .    /    rsft
    lctl lmet lalt           spc            ralt rmet cmp  rctl
  )
  ```

  See the [keyboard templates](../keymap/template) for further
  inspiration.

+ `deflayer`: defines a layer to be used later.

   For example, defining a qwerty layer, as well as one for special
   symbols and numbers:

  ```
  (deflayer qwerty
    grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
    tab  q    w    e    r    t    y    u    i    o    p    [    ]    \
    caps a    s    d    f    g    h    j    k    l    ;    '    ret
    lsft z    x    c    v    b    n    m    ,    .    /    rsft
    lctl @num lalt           spc            ralt rmet @sym @kil
  )

  (deflayer numbers
    _    _    _    _    _    _    _    _    _    _    _    _    _    _
    _    _    _    _    _    XX   /    7    8    9    -    _    _    _
    _    _    _    _    _    XX   *    4    5    6    +    _    _
    _    _    \(   \)   .    XX   0    1    2    3    _    _
    _    _    _              _              _    _    _    _
  )
  ```

## Operating on Layers

There are many buttons that can operate on layers.

+ `layer-toggle`: toggles to a specific layer as long as the key is held.

  ```clojure
  (defalias ket (layer-toggle toggled-layer))
  ```

+ `layer-delay`: change to the layer temporarily for the given amount of
  time (in ms).

  ```clojure
  (defalias ked (layer-delay 500 delayed-layer))
  ```

+ `layer-next`: the next key press is handled by another layer
  (leader-key style).

  ```clojure
  (defalias ken (layer-next next-layer))
  ```

+ `layer-switch`: change the base layer; i.e., deregister the
  bottom-most layer and swap it out with another one.

  ```clojure
  (defalias kes (layer-switch switch-layer))
  ```

+ `layer-add` and `layer-rem`: overlay a layer on top of the current
  layer resp. remove the overlayed layer from the current layer.

  NOTE: The above two commands can be particularly dangerous if used
  alone and should really only be used together. Be careful that you
  don't make a configuration in which you can't switch back to your base
  layer!

  ```clojure
  (defalias
    add (layer-add multi-overlay)
    rem (layer-rem multi-overlay)
  )
  ```
