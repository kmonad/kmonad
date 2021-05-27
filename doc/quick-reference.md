# Introduction
Welcome to the KMonad quick reference guide. This document aims to provide a
simple definition and a usable example for all possible configuration options in
the KMonad program to act as a quick reference that you look into when forgetting
a function or/and wanting to know something quickly. Notice that this document
does not try to be a full guide or a detailed documentation, this is the aim of
[tutorial.kbd](https://github.com/kmonad/kmonad/blob/master/keymap/tutorial.kbd).

# Define Configuration

+ `defcfg`: configuration rules (input and output devices, etc).

+ `input`: define the input keyboard which the program will captures.  
+ `output`: define the output keyboard which the system will rely on.

	- For Linux:
       ```clojure
       input  (device-file "/dev/input/by-id/usb-04d9_daskeyboard-event-kbd")
       output (uinput-sink "My KMonad output")
       ```
	- For Windows:
       ```clojure
       input  (low-level-hook)
       output (send-event-sink)
       ```
	- For MacOS:
		```clojure
		input  (iokit-name "my-keyboard-product-string")
		output (kext)
		```
+ `fallthrough` (boolean, defaults to `false`): allow all keys, even if not defined
in defsrc.

+ `allow-cmd` (boolean, defaults to `false`): allow arbitrary shell commands to
  be run as keys.  
NOTE: This is very dangerous since something like binding `rm -rf /*` to a key
and executing it on a key press is possible.

+ `cmp-seq` (key, defaults to `RightAlt`): compose key for Unicode input (Linux X11
specific).

	+ `cmp-seq-delay` (number): delay between each pressed key in a compose-key
sequence
```clojure
(defcfg
  ;; For Linux
  input  (device-file "/dev/input/by-id/usb-04d9_daskeyboard-event-kbd")
  output (uinput-sink "My KMonad output" "/run/current-system/sw/bin/sleep 1 && /run/current-system/sw/bin/setxkbmap -option compose:ralt")

  cmp-seq ralt
  cmp-seq-delay 5

  fallthrough true

  allow-cmd true
)
```
## Layers, aliases and buttons

+ `defsrc`: define physical keyboard input keys, although its layout is very
  similar to a layer, it is not a one and will not be used as a one.
```clojure
(defsrc
  grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
  tab  q    w    e    r    t    y    u    i    o    p    [    ]    \
  caps a    s    d    f    g    h    j    k    l    ;    '    ret
  lsft z    x    c    v    b    n    m    ,    .    /    rsft
  lctl lmet lalt           spc            ralt rmet cmp  rctl
)
```

+ `defalias`: define a button, can be referenced later using `@` in `deflayer`
```clojure
(defalias
  num  (layer-toggle numbers) ;; Bind num to a button that switches to a layer
  kil  C-A-del                ;; Bind kil to a button that Ctrl+Alt+deletes
)
```

+ `deflayer`: define as many layers as you please (to toggle, add or remove).
```clojure
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

+ `layer-toggle`: toggles to a specific layer as long as the key is held.
```clojure
(defalias ket (layer-toggle toggled-layer))
```

+ `layer-delay`: change the layer temporarily
```clojure
(defalias ked (layer-delay 500 delayed-layer))
```
+ `layer-next`: next key press is handled by another layer (leader-key style)
```clojure
(defalias ken (layer-next next-layer))
```
+ `layer-switch`: change the base layer
```clojure
(defalias kes (layer-switch switch-layer))
```
+ `layer-add`: overlays a layer on top of the current layer  
+ `layer-rem`: removes the overlayed layer from the current layer  
NOTE: Using them is very dangerous, it is easy to make configurations which breaks the keyboard!
```clojure
(defalias
  add (layer-add multi-overlay)
  rem (layer-rem multi-overlay)
)
```

## Multi-use buttons

+ `around`: combines a bunch of keys into one key 
```clojure
(defalias ad (around alt del)) ;; this is like pressing Alt+Del 
```
### Modded buttons
`C-` : `(around lctl X)`  
`A-` : `(around lalt X)`  
`M-` : `(around lmet X)`  
`S-` : `(around lsft X)`  
Then `RC-`, `RA-`, `RM-`, and `RS-` behave exactly the same, except using the
right-modifier.
```clojure
(defalias Ca C-a) ;; this is equivalent to (defalias Ca (around Ctl a))
```

+ `sticky keys`: will act like the key is held temporarily after just one press
```clojure
(defalias slc (sticky-key 500 lctl))
```

+ `tap-macro`: takes a sequence of keys and taps them with an optional delay  
+ `pause`: pause temporarily
```clojure
(defalias
  ta1 (tap-macro K M o n a d)
  ta2 (tap-macro K M o n a d :delay 5)
  ta3 (tap-macro K P5 M P4 o P3 n P6 a (pause 5) d) ;; P5 = (pause 5)
)
```

### Tap buttons

+ `tap-next`: combines 2 buttons, one for tapping and one for holding
```clojure
(defalias tan (tap-next a sft))
```
+ `tap-hold`: like tap-next but with a timeout for tapping, if the key is
released before the timeout it taps after the timeout it is turns into holding
```clojure
(defalias tah (tap-hold 200 a sft))
```
+ `tap-hold-next`: like tap-next but it held after a period of time
```clojure
(defalias thn (tap-hold-next 1000 a sft))
```
+ `tap-next-release`: like tap-next but it decides whether to tap or hold based
on the next release of a key
```clojure
(defalias tnr (tap-next-release a sft))
```
+ `tap-hold-next-release`: like tap-next-release but it held after a period of
time like tap-hold-next
```clojure
(defalias thr (tap-hold-next-release 1000 a sft))
```

+ `multi-tap`: combines a sequence of keys into one key with a timeout between
each key
```clojure
(defalias mt  (multi-tap 300 a 300 b 300 c 300 d e))
```
+ `around-next`: perform the next button-press inside some context (like
layer-next but more generalized)
```clojure
(defalias ns  (around-next sft)  ;; Shift the next press
```
+ `cmd-button`: takes two arguments, the second one of which is optional. These
represent the commands to be executed on pressing and releasing the button
respectively
```clojure
(defalias
  dat (cmd-button "date >> /tmp/kmonad_example.txt")   ;; Append date to tmpfile
  pth (cmd-button "echo $PATH > /tmp/kmonad_path.txt") ;; Write out PATH
  ;; `dat' on press and `pth' on release
  bth (cmd-button "date >> /tmp/kmonad_example.txt"
                  "echo $PATH > /tmp/kmonad_path.txt")
)
```
