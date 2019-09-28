# Configuration guide

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
There is order requirement to a configuration file. It is possible to reference
aliases before defining them, for example, as long as they are defined at some
point.

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
[README](https://github.com/david-janssen/kmonad/blob/master/README.md).

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
To specify an alias, assign to an `@`-prefixed name consisting of alpha-numeric
characters. Assignment is performed using the `=` symbol. What follows the `=`
symbol must be a valid button identifier.

For example:
```
@num = LT-numpad       // A button that switches to the numpad layer
@ca  = TH 300 lctl a   // A taphold that is either a control, or an a
@lb  = TH % LT-symbol  // A taphold that either switches to a layer, or emits a '%'
```

It is illegal to use a `transparent`, or another alias in an alias definition.
Either of these will fail:
```
@foo = _
@bar = @num
```

If you refer to an undefined alias in a layer, KMonad will error out before
starting up.

### Alias reference
To reference to an alias, simply include the defined name, including the `@`, in
the layer as you would any other button. For example:
```
LAYER example
  a    b    c
  d    @foo e
```

