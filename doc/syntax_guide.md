# Syntax guide

For an overview of the general concepts underpinning KMonad, see the [general
concepts documentation](concepts.md).

Note that KMonad is still very much under active development and I am learning a
lot about syntax. Therefore please remember that there are currently **no
guarantees** of syntactic backward compatibility. If you update, please check
for syntax changes and bear with me. Once we go to version 1.0.0 backward
compatibility will be maintained.

## General syntax

### Comments
We currently support `//`-style line-comments and `/* ... */`-style
block-comments.

### Order
There is no order requirement to a configuration file. It is possible to
reference aliases before defining them, for example, as long as they are defined
at some point.

There is one aspect where ordering is **important**: The first layer you define
in a configuration file will be the layer that is loaded when KMonad starts.

### Alignment
To avoid cluttering the code with a lot of explicit lists and commas, KMonad
currently uses alignment between characters to indicate button correspondence,
(more about this below). This means that it is probably highly beneficial to use
at least a text-editor that has a fixed-width font, and ideally a column
indicator when editing configuration files.

To aid in creating neatly aligned maps, KMonad ensures that any possible button
or keycode can be parsed by an identifier no longer than 4 characters. Therefore
aligning your maps in 5-character wide columns should allow you to fit a lot of
information in very little space.

## IO
KMonad internally translates 1 type of keyboard event to another type of
keyboard event, but it needs a way to:
1. Read (and capture) events from the operating system
2. Emit events back to the operating system.

At the moment KMonad only supports 1 interface for each of these 2 capacities,
and the syntax to define these inputs is therefore very simple. 

### Input
#### Linux
To register an input, KMonad reads binary data from a `/dev/input` device, and
uses the `IOCTL` call to ensure the events do not get transmitted anywhere else.
The syntax to define which file to open is:

```
INPUT = LINUX_DEVICE L64 /dev/input/by-id/my-keyboard
```

In the future, if we specify other ways of grabbing keyboards (perhaps a Windows
method?) we will expand the range of possible input constructors.

The `L64` signifies which decoding strategy to use. I must admit some ignorance
here: I simply figured out how to parse keyboard events from the stream of
binary data by reverse engineering it on my system, and I do now know how
portable this is. I am currently assuming that the decoder linked to `L64` works
on other 64-bit linux systems. The code is designed so that it is easy to
specify different decoders for different systems.

#### Windows
Under Windows we install a low-level keyboard hook with a callback that
intercepts input keyboard events and sends them to KMonad. This IO interface is
currently not configurable in any way, therefore the windows input syntax is
simply:

```
INPUT = LL_KEYBOARD_HOOK
```

### Output
#### Linux
We currently use the `uinput` subsystem to create a simulated keyboard over
which we emit keyevents back to the OS. To that extent, the user needs to have
permissions, and the `uinput` subsystem has to be loaded. For information on how
to make this happen, see the [README](../README.md#uinput-permissions).

Since there is only 1 supported output method at the moment, the basic syntax is
very simple:
```
OUTPUT = UINPUT_SINK
```

Additionally, we have two optional positional arguments. First, it is possible
to specify a name for your generated keyboard, helping you distinguish multiple
instances of KMonad for different devices. A name is provided simply as a string
between "".

```
OUTPUT = UINPUT_SINK "My Keyboard"
```

Secondly, there is an optional 'post-init' shell-command that KMonad will fork
off to be run. This is very useful if you intend to use the support for
special-characters, as depending on your configuration, X11 might have to be
informed about what to consider a 'Compose' key. Note that there is a little bit
of time between the UinputDevice being created, and actually being registered by
the OS, so it is necessary to include a small delay in your command.
Additionally, note that currently the only 'Compose' key we support is
right-Alt. This will be expanded on in the future.

The command is forked, so KMonad will start immediately, but not until the
forked command has run will any special-character macros evaluate correctly. 

Note as well that, at the moment, if you want to specify a post-init command,
you are also forced to specify a custom name. Also, new-lines are not supported
yet in the `OUTPUT` definition.
```
OUTPUT = UINPUT_SINK "My Keyboard" "/usr/bin/sleep 1 && /usr/bin/setxkbmap -option compose:ralt"
```

#### Windows
Windows currently supports 1 method of simulating keyboard events using the
`Win32` `SendInput` API-call. This currently does not come with any
configuration options, so the way to configure the output under windows is
simply:
```
OUTPUT = SEND_EVENT_SINK
```

## Aliases
Since we use alignment to indicate button correspondence, it is very cumbersome
to have to deal with long button definitions. Therefore, KMonad provides syntax
to define and reference aliases, which are simply names for button definitions.

### Alias specification
To specify an alias, assign to an `@`-prefixed name consisting of alpha-numeric,
punctuation, or symbolic characters. Assignment is performed using the `=`
symbol. What follows the `=` symbol must be a valid button identifier. The name
can overlap with valid button identifiers, like `spc`, and KMonad can tell the
difference between `spc` and `@spc`. The name ends when a space is encountered,
and the name must be followed by an `=`

For example:
```
@num = LT-numpad       // A button that switches to the numpad layer
@ca  = TH 300 a lctl   // A taphold that is either a control, or an a
@lb  = TH % LT-symbol  // A taphold that either switches to a layer, or emits a '%'
@:-) = || : - ) ||     // A macro that emits a smiley
```

It is illegal to use a `transparent`, or another alias in an alias definition.
Either of these will fail:
```
@foo = _
@bar = @num
```

If you refer to an undefined alias in a layer, KMonad will error out before
starting up. Additionally, duplicate alias definitions (multiple assignments to
the same name) will also cause an error.

### Alias reference
To reference to an alias, simply include the defined name, including the `@`, in
the layer as you would any other button. For example:
```
LAYER example
  a    b    c
  d    @foo e
```

## Layers
A concept central to KMonad configuration is the definition of layers. A layer
is simply a set of buttons (or keycodes in the case of the `SRC` layer),
prefixed with a header that contains metadata on the layer.

### Source layer
Every valid configuration file must include 1 and exactly 1 `SRC` layer. This
`SRC` layer defines the original layout of the keyboard before KMonad does any
remapping. Any button in a button layer defined later must line up exactly with
the column index of leftmost character of the keycode name in the SRC layer.

Note that the arrangement of your `SRC` layer does not actually have to
correspond to the physical alignment of your actual keyboard. It can even
include keycodes that don't occur on your keyboard, although it then won't be
possible to trigger any buttons linked to that keycode.

Additionally, it is not necessary to align your SRC layer itself in columns,
although I personally think it leads to much more readable configurations.

Some examples: (Note, this file would error due to it containing 3 `SRC` layers,
but each layer itself is fine).
```
// This is perfectly valid
SRC
  q    w    e    r    t
  a    s    d    f    g

// So is this
SRC
  q    w    e    r    t
    a    s    d    f    g
    
// Or this (although it seems impractical):
SRC
    q    w  e r     d   y
  s    l        o      g
```

Not providing a `SRC` layer, providing more than 1 `SRC` layer, or including
duplicate keycodes in your `SRC` layer will all result in an error.

To see exactly what qualifies as a valid keycode, see below.

### Button layer
Button layers are specified in exactly the same way as `SRC` layers, except with
the `LAYER` keyword, and they allow for a slightly more complicated header.
First of all, every layer must provide an alpha-numeric name after the `LAYER`
keyword, and it is that name which allows buttons to switch to the layer.
Additionally, there is a possibility to introduce metadata after the name that
provides the ability to customize how to interpret the layer.

Currently, the only keyword that can be passed is `anchor`, which must be
immediately followed by a valid keycode identifier that references a keycode
that exists in the `SRC` layer. Exactly what anchoring does is explained below.

Header examples
```
LAYER dvorak                      // Valid
LAYER qwerty ~ anchor k           // Valid, provided k exists in the SRC layer
LAYER                             // Invalid, LAYER's must be followed by a name
LAYER qwerty ~ anchor k, anchor q // Invalid, Duplicate anchors not allowed
```

After the header, just like in the `SRC` layer, a button layer must specify a
grid of valid button identifiers. To see exactly what qualifies as a valid
button, see below.

Every button in the grid must line up exactly with a corresponding keycode in
the `SRC` layer.

For example
```
SRC
  q w e r t
  a s d f g

LAYER colemak
  q w f p g
  a r s t d
```
Or:
```
SRC 
  q  w  e  r  t
   a  s  d  f  g

LAYER colemak
  q  w  f  p  g
   a  r  s  t  d
```

### Advanced alignment
When KMonad parses a matrix of keycodes or of buttons, it normalizes the
coordinate system so that the top-left most point is set to (0, 0) (Note that
this does not necessarily have to correspond to an existing key). This means
that the indentation between two grids does not have to be the same, although it
certainly helps keep things clear. 

Also, note that a `LAYER` does not need to map a button to every keycode in the
source, it is perfectly fine to omit certain keys.
```
SRC
  q  w  e  r  t
  a  s  d  f  g
 
// This is fine
LAYER foo
  n  e     o  ;
  j  u        u
 
// So is this
LAYER bar
b  f  s
l  i  -

// But not this
LAYER baz
u  i  n
  n  i  e
```

An example where the top-left point does not correspond to a key:
```
SRC
       q w e r
  caps a s d f
```

You can use the `anchor` keyword to shift the top-left coordinate of a buttonmap
so that it lies exactly on where the provided keycode lies in the `SRC` map. For
example:

```
SRC
  esc  f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12 
  grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
  tab  q    w    e    r    t    y    u    i    o    p    [    ]    ret
  caps a    s    d    f    g    h    j    k    l    ;    '    \
  lsft      z    x    c    v    b    n    m    ,    .    /    rsft
  lctl      lmet lalt           spc            ralt rctl 

// This is fine, and creates a numpad where the 7 corresponds to the `u`
LAYER numpad ~ anchor u
7    8    9
4    5    6
1    2    3    0

// This is also fine, and arguably more clear
LAYER numpad2 ~ anchor u
                                     7    8    9
                                     4    5    6
                                     1    2    3    0

// This is a way to achieve the same thing without using an anchor
LAYER numpad3
  _

                                     7    8    9
                                     4    5    6
                                     1    2    3    0
```

A good practice is using the `transparent` button to mark every single button
location in a map, even if you only want to specify a few buttons. It provides
for very clear and readable configurations. For an overview of this technique,
consult the configuration [for my daily driver](../example/atreus.kbd).

## Parsing keycodes
Many keycodes have various ways to refer to them.

First, the names used for keycodes lines up with those defined in [the linux
headers](https://github.com/torvalds/linux/blob/master/include/uapi/linux/input-event-codes.h),
except without the underscores, and in lowercase. So, for example `keya` refers
to keycode for `a`.

Second, all of the aforementioned names also work with the prefix `key` removed.
So `a` simply refers to the keycode for `a`, `leftbrace` works for left brace,
and `102nd` works for `102nd`.

Third, we try to provide at least 1 name for each keycode that is no longer than
4 characters long (work in progress). To that extent, there are various
abbreviations that I will document nicely in the future, but for now I refer you
to [the source
code](https://github.com/david-janssen/kmonad/blob/master/src/KMonad/Core/Parser/Parsers/KeyCode.hs).
Note that `KeyBackslash` is mapped to "\\", but that is just Haskell needing to
escape a "\", so a `\` is the symbol that refers to backslash.

Additionally, note that there simply are no keycodes for symbols like `%`, `&`,
or `#`. These are simply shifted versions of other keycodes.

## Parsing buttons, and types of buttons

### Emit
An `emit` button is exactly what you would expect of a button. You push it, and
it emits a keypress event for some keycode, you release it, and it emits a
keyrelease event for that same keycode. Nothing more, nothing less. Note that we
do not deal in key-repeat events, and just let the OS take care of that.

Since an `emit` simply wraps a keycode, anything that parses as a keycode in a
`SRC` layer parses as an `emit` of that keycode in a `LAYER` layer.

### Modded versions
We use the standard emacs-style syntax to indicate modded versions of basic key
events. Like this (note this is not valid syntax, buttons need to exist in a
`LAYER` or alias definition):
```
// Simple modded emits, defaulting to the 'Left' version of the modifier
C-a   // Control-a
S-b   // Shift-b
A-c   // Alt-c
M-d   // Meta-d (Meta is usually the Gui button)

// To specifically use the right modifier, prefix with an R
RC-a
RS-b
RA-c
RM-d

// A modded specification can mod either an emit, or another modded
C-S-x        // Control-shift-x
RS-A-b       // Rightshift-alt-b
C-S-M-A-l    // Control-shift-meta-alt-l
```

Additionally, since it is probably very likely that you might want to emit very
common symbols that are just shifted versions of other keycodes, we define a lot
of abbreviations:

All of the shifted numbers count for themselves, so `!` is the same as `S-1`,
etc. `>` is `S-,`, `{` is `S-[`, etc. There is one **major** exception. `_` does
not correspond to `S--`, but instead to `transparent` (see below). To shortcut
an `S--`, use `__` instead.

Additionally, shifted letters also evaluate to macros of themselves, so an `A`
in a list will evaluate to a macro producing an `A`.

Note that currently the `!` and `A` style macros evaluate to complete macros
that trigger upon a press. That means that you can't hold them and get a series
of repeating `A`'s or `!`'s. This will be fixed in the future.

### Transparent
Since it might in certain cases be clearer to signal that a particular keycode
exists in a certain place, but that you do not want to handle it, you can
specify a `transparent` button. All this does is pass the event down the
layer-stack. If this is the last layer in the stack, then the event is simply ignored.

To signal a `transparent` button, either use the names `transparent`, `trans`,
or simply an `_`. Note that this is exactly the same as not specifying a button
in that location at all.

Adapted from the example for advanced alignment:
```
SRC
  q  w  e  r  t
  a  s  d  f  g
 
// This is fine
LAYER foo
  n  e     o  ;
  j  u        u
  
// This is equivalent, and perhaps clearer
LAYER bar
  n  e  _  o  ;
  j  u  _  _  u
```

### Block
If you do not want to do anything for a particular keycode, but neither do you
want any potential other layers lower down the stack to handle the event, you
can signal that an event should be blocked by adding a block-button to that
location, using either the word `block` or simply a double capital `XX`

For example
```
SRC
  a  b  c  d
 
// We:
// 1. Pass the 'a' down the stack
// 2. Block the 'b' altogether
// 3. Turn the 'c' into 'emiting an underscore'
// 4. Turn the 'd' into 'emiting a d'
LAYER foo
  _  XX __ d

```

### Macros

We provide full support for emitting sequences of key-presses and releases in a
macro. Various other buttons are implemented as macros as well, like
special-symbol emit-sequences or currently characters like `%` or `A`. It is,
however, possible to define your own completely arbitrary macros.

Since `(` evaluates to "emit a `(`", we enclose our macros in double parens
instead. Inside double parens we support a special syntax. Any character on its
own evaluates as either a tap of that button (like `a`) or the macro required to
tap that button (like `%`) Additionally, `Px` will evaluate to 'Press x' where 'x'
must be a **keycode**, therefore `P%` is illegal, whereas `P.` is perfectly fine
(support will be expanded in the future). The same is true for `Rx` and
releases. If you want to press a shifted button, you can simply use `Plsft P5`
to achieve `P%`-style behavior.

If you use the press-only or release-only style sequences in your macros it is
intirely your own responsibility to make sure you leave the keyboard in a
neutral state when you finish (or not, if that is what you want). It is
perfectly possible to leave your keyboard in a state of constantly pressing `a`
by using a `Pa` and never releasing it again.

```
@>>= = (( > > = ))
@foo = (( H e l l o spc w o r l d ))
@forevera = (( Pa ))
```

Note, this might go without saying, but although it is possible to encode your
security paswords into your keyboard driver so you can trigger them with just a
button, this is very much **not recommended**, since it seriously compromises
your security.

### Layer-Toggle
The Layer-Toggle manipulates the layer-stack. When pressed, it adds the layer it
refers to to the top of the stack, and when released, it removes that layer from
the stack again. Note that the layer-stack determines which button handles a key
*press*. The key release is **always** handled by the button that initiated the
press. So if you mask a button with another button in the middle of tapping it,
it is still the original button that will handle the release, although any
future press will be passed to the button that has just been mapped on top. 

```
@num = LT-numpad  // An alias is nice to keep things short, but not required

SRC
  q    w    e    r
  a    s    d    f

LAYER home
  q    w    p    g
  @num LT-a s    t 

// Switching to numpad with @num is fine, it turns qwer into 1234
LAYER numpad
  1    2    3    4
  _    _    _    _

// Switching to a with LT-a works as well.
LAYER a
  _    _    _    _
  5    6    7    8
```

### Layer-Add and Layer-Rem
The Layer-Add button manipulates the layerstack by adding a predetermined
layer-id to the top of the layerstack every time it is pushed. Layer-Rem does
exactly the opposite. Note that this makes it possible to permanently alter and
screw up your stack-state.

The LA and LR buttons are provided as primitives for stack-manipulation. They
let you do what you want, but come with less safety guarantees. More elegant
ways of dealing with this might be implemented soon.

```
@anm = LA-num
@rnm = LR-num

SRC
  q    w    e    r
  a    s    d    f

LAYER home
  @anm @rnm _    _
  _    _    _    _
  
LAYER numpad
  _    _    _    _
  1    2    3    4
```

In the above example, if you press `q` once, the bottom row will then function as
numbers. If you press `w`, everything is back to normal again. If you press `q`
twice, though, you will have to press `w` twice as well to restore normal
functioning. This is the most naive way of doing this.

```
@anm = LA-num
@rnm = LR-num

SRC
  q    w    e    r
  a    s    d    f

LAYER home
  @anm _    _    _
  _    _    _    _
  
LAYER numpad
  @rnm _    _    _
  1    2    3    4
```

The above version is a much more stable way of dealing with this. The button to
remove the numpad exists **only in the numpad layer**. Additionally, it masks
the 'add numpad' button, so it becomes impossible to switch to numpad more than once.

```
@rhm = LR-home

SRC
  q    w    e    r
  a    s    d    f

LAYER home
  @rhm a    b    c
  _    _    _    _
```

The above version is perfectly legal. Press q once and you will never handle an
event ever again until you restart KMonad.

### Tap-Hold
The tap-hold button allows for a button to do 2 different things, based on
wether the button is held or tapped. Tapholds are defined by, in order:
1. The keyword `TH`
2. The duration after which it switches to 'held' status (alternatively, the
   time you have to release a TapHold to get it to register a tap), in
   milliseconds.
3. The button to use when tapping
4. The button to use when holding

Perhaps it is easiest to start immediately with an example:
```
@th  = TH 300 tab LT-num   // tap -> emit a Tab, hold -> switch to `num` layer
@xcp = TH 400 esc lctl     // tap -> emit an Esc, hold -> function as Control
```

The tap-hold pauses all event processing until it decides what kind of button it
is, and then emits either the tap of its tap-button (when tapping) or the press
of its hold button (when holding), with the release coming when the tap-hold is
released again.

Take for example:
```
@ts = TH 500 q lsft  // tap -> emit q, hold -> emit LeftShift
```
Lets assume we bind `@ts` to caps-lock and bind all other keys simply to
emit-versions of themselves. Now, if we press capslock, then press "asdf", and
release capslock again, before the 500 ms are up, we will notice a slight pause
in processing, and the moment we release caps-lock, we will type "qasdf". If,
instead, we do the same, but keep capslock pressed, after exactly 500ms, we will
see "ASDF", processing of events will not be paused and occur normally, in the
context of 'shift' currently being pressed (i.e., the shift won't stop, it will
now simply be a normal shift key). When you then release capslock, KMonad will
signal a release of 'shift' to the OS.

Note that when you bind a taphold to a very common key, like the spacebar, it
might take some getting used to, because tapping it will always come with just
the tiniest of delays, because we can only emit a tap of the spacebar upon
*release* of the spacebar.

### Tap-Next
The tap-next button is another way to have a button do 2 different things. Since
a Tap-Hold button works around a timed delay, if you for example have a `TH 300
esc lctl`, then if you want to use this to press `C-t`, you are going to have to
wait for 300 ms for that signal to register. If you release too early, you will
get an `esc, t` instead.

The tap-next button, however, simply waits to see if the next event is releasing
itself, in which case it taps, or pressing another button, in which case it is a
mod. Additionally, as a small extra tweak, it ignores releases of other buttons.
So if you, for example, press `e`, press the `TapNext`, release `e`, and then
press `t`, the release of the `e` will have no effect on anything, and the
`TapNext` will register as holding, since the next event was a pressing of a
`t`. This is simply to smoothe out tap-detection when typing fast, since you do
not have to make sure you have everything released before pressing a TapNext.

A tap-next is parsed as, in sequence
1. The symbol `TN`
2. The button to push (allows emits and modded)
3. The button to hold (allows emits, modded, and layer-toggles)

For example:
```
@xcp = TN esc lctl   // Esc when tapped, control when held
@foo = TN q LT-syms  // q when tapped, goto syms when held
```

#### TapNext vs TapHold
This is purely a matter of personal taste, but I prefer TapHold buttons for
buttons that I often use in quick succession when typing. If you are going to
bind a LayerToggle to a homerow key like `s`, for example, then if you use a
TapNext, then unless you type very carefully, you are very often going to
trigger a Layer change when typing a word with an `s` in. Were you to use a
TapHold, however, the only way you could accidentally trigger a layer-change
would be to hold that button longer than your specified delay, which (depending
on your delay) is very unlikely when typing at a decent clip.

On the other hand, a TapHold button can be a bit sluggish to trigger its 'held'
state for those button-combinations that you type very quickly. I bind an XCape
(Control when held, escape when tapped) to my caps-lock key. If I use that to
open a new tab in my browser with "C-t", I often 'miss' the delay because I type
this so quickly. Here a TapNext is much better, since it does not matter how
quickly you 'complete' your modded combination.

In summary: TapHolds are nice for buttons that you want to be conservative in
their 'becoming held' property, and TapNext's are nice for buttons that you want
to be able to use as 'becoming held' very quickly. Of course, the delay you
choose for your TapHold button also greatly affects this.

### MultiTap
The multi-tap button is a button that decides what to do based on how often it
is tapped in quick succesion. Currently it only supports tapping, although I
might implement just holding (and later catching a release) in the future.

A multitap is specified by, in order
1. The `MT` symbol
2. 1 or more occurences of some button and some number of milliseconds to wait
3. 1 button

For example:
```
// q if tapped once, or esc if tapped again within 200 ms
@qsc = MT q 200 esc 

// emit various letters of the alphabet, with the window to get in another tap
// shrinking
@foo = MT a 500 b 400 c 300 d 200 e 100 f 50 g
```

The buttons that can exist in a multitap are either emits or modded.

### Lock buttons
The 3 different lock-buttons, ScrollLock, CapsLock, and NumLock are handled a
little bit differently from normal emitting keys. KMonad keeps track of what
state the 3 locks are currently in and provides 3 different actions: Engaging a
lock (does nothing if the lock is already on), releasing a lock (does nothing if
the lock is already off), and toggling a lock, which behaves like a lock-key
does normally.

Engaging a lock is defined by: `LON-` followed by the name of the key,
disenganging the lock is defined by `LOFF-` followed by the name. Finally,
simply the name of a lock-button is automatically mapped to the correct toggle
version.

NOTE: This means you need to make sure all 3 locks are **off** when starting KMonad.

For example:
```
@con = LON-caps
@cof = LOFF-caps
@ctp = MT LOFF-caps 200 LON-caps

SRC
  a    s    d    f    g
  
LAYER test
  @con @cof caps @ctp g
```

Now:
1. Pressing `a` will always engage caps-lock, regardless of whether capslock is
   on or not.
2. Pressing `s` will always disengage caps-lock, regardless of whether capslock
   is on or not.
3. Pressing `d` will toggle caps-lock, like a normal caps-lock would.
4. Tapping `f` once will disable caps-lock, tapping `f` twice will enable caps-lock
5. You can press `g` to emit a `g` to see that things are indeed locked or not.

### Special character support
As indicated above, special characters are nothing more than macros. There *are*
no real special characters at the level at which KMonad operates, however
specifying special-characters is so close to KMonad's domain that we do try to
provide good support.

Support currently only exists for special characters that can be emitted by
X11's compose-key sequences. Support for general Unicode is on the books.

Essentially, any special character that has an XLib defined compose-key sequence
can most simply be defined as itself. For example, given that you have
configured KMonad and X11 correctly, the following should just work:

```
SRC
  a    s    d    f

LAYER example
  é    ë    ñ    Æ
```

For an overview of *all* the supported special characters, the best place to
look at the moment is [the source code](../src/KMonad/Core/Parser/Parsers/SpecialSymbol.hs).

Additionally, we provide support for a variety of 'hidden-key' style
modify-the-next-keystroke behavior (again, using X11's macros). These can be
specified by using the `+` character, followed immediately by the keycode (or
shifted keycode) to trigger it. For example:

```
SRC
  a    s    d    f

LAYER example
  +'   +"   +o   e 
```
Here:
- [a, f] -> é
- [s, f] -> ë
- [d, f] -> œ

For info on getting special character support working, consult [the
README](../README.md). 

### Sequencing buttons
For those buttons that make sense in the context of 'only tapping' (i.e., not
TapHold buttons, or multi-tap buttons), there exists the possibility of
sequencing them. This is conceptually different from a Macro button in the
following way: A macro is 1 button that gets 1 press and then emits a sequence
of outputs.  Sequencing buttons turns 1 physical press of a button into a number
of virtual taps of any number of buttons.

For this reason, for example, it is currently impossible to include
layer-toggling functionality in a Macro (macros only sequence outputs). To
support sequencing arbitrary actions we provide an 'after' button that is
created like this:

```
@ab  = >> a b      // Tap an emit-a button, then an emit-b button
@abc = >> a >> b c // Tap an emit-a, emit-b, and an emit-c button
```

This can be especially useful if you want to have a layer with various actions
that you 'switch into', but then want to switch out of that layer automatically
after 1 such action is completed. For example:

```
...

@to  = LT-test

@foo = (( M-w 1 ))            // Performs macro
@bar = >> (( M-w 2 )) LR-test // Performs macro and leaves layer
@baz = >> (( M-w 3 )) LA-more // Performs macro and adds other layer
                              // to the stack

SRC 
  a    s    d

LAYER base
  @to  _    _

LAYER test
  @foo @bar @baz

LAYER more
  XX   XX   XX

```
