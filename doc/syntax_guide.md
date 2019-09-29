# Syntax guide

## Basic concepts
The KMonad configuration language tries to provide the cleanest possible
configuration syntax for specifying keyboard remappings. It does this by making
nearly everything a button or token, and removing nearly all syntax. The syntax
constructs that do exist are nearly always introduced by capital letters, or the
'@', '-', '=', or '_' symbols.

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

### Output
We currently use the `uinput` subsystem to create a simulated keyboard over
which we emit keyevents back to the OS. To that extent, the user needs to have
permissions, and the `uinput` subsystem has to be loaded. For information on how
to make this happen, see the
[README](https://github.com/david-janssen/kmonad/blob/master/README.md#uinput-permissions).

Since we currently only allow exactly one way to specify output, the syntax is
simply:
```
OUTPUT = UINPUT_SINK
```

I am considering adding the ability to specify a name for your uinput sink in
the future.

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
location, using either the word `block` or simply a capital `X`

For example
```
SRC
  a b c
 
// We pass the 'a' down the stack, block the 'b' altogether, and turn the c to a q
LAYER foo
  _ X q

```

### Tap-macros
KMonad provides support for macros that consist of tapping other buttons. There
is currently no support for pressing one button, tapping another, and then
releasing again, only consecutive taps. Tap-macro buttons trigger their output
sequence on press, and do nothing on release.

To define a tap-macro, enclose a sequence of emit or modded buttons in double
`||`.

```
@>>= = || > > = ||
@foo = || H e l l o spc w o r l d ||
```

Note, this might go without saying, but although it is possible to encode your
security paswords into your keyboard driver so you can trigger them with just a
button, this is very much not recommended, since it seriously compromises your
security. 

### Layer-Toggle
We currently provide 1 button that manipulates the layer-stack. The
layer-toggle. When pressed, it adds the layer it refers to to the top of the
stack, and when released, it removes that layer from the stack again. Note that
it is currently perfectly possible to get yourself completely stuck by mapping a
button over your layer-toggle that you then switch to. This will be remedied in
the future.
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

// Switching to a with LT-a works, but never allows you out again, because
// releasing `s` (LT-a) is now bound to releasing the button 6.
LAYER a
  _    _    _    _
  5    6    7    8
```

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
