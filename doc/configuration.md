# Configuration

KMonad is configured with a single configuration file with a lisp-like syntax.
For examples of this syntax, please consult the [templates](../templates)
folder.

## Comments
KMonad supports lisp-like `;;` line comments and `#|` ... `|#` block comments.
Unlike lisp, a single `;` will not serve as a comment (instead, it designates a
semicolon button or keycode).

## Basic structure
A valid KMonad configuration consists of a number of mandatory blocks, plus
additional definitions. Each configuration must contain:
- Exactly 1 [(defcfg ...)](./configuration.md#defcfg-block) block
- Exactly 1 [(defsrc ...)](./configuration.md#defsrc-block) block
- Any number of [(defalias ...)](./configuration.md#defalias-block) block
- 1 or more [(deflayer ...)](./configuration.md#deflayer-block) blocks

### `defcfg` block
The `defcfg` block contains some basic settings like how to read events from the
OS and how to send events to the OS. Each configuration option is named, and its
value must follow its fixed name. The minimal complete definition includes an
`input` and an `output` value. The `defcfg` block is designed to allow future
addition of different IO options, but at the moment it does not provide many
choices.

#### Linux
Under Linux, the only valid values are:
- `input`:  `(device-file "/path/to/file")`.
- `output`: `(uinput-sink "Name of keyboard" "optional post-init command")`

#### Windows
Under Windows the `defcfg` block is even simpler, you have no options.
- `input`: `(low-level-hook)`
- `output`: `(send-event-sink)`

#### Mac
Under Mac, the current valid values are:
- `input`: `(iokit-name "optional product string")`
- `output`: `(kext)`

By default, kmonad will modify input from all keyboards. If a product
string is provided, kmonad will modify input from all keyboards whose
product string matches the given product string.

To determine the product string of the connected keyboards, you can
run:
```shell
cd c_src/mac
make
./list-keyboards
```

Note: Apple keyboards support a function-lock feature by default,
where the function keys have special behaviors when held with
<kbd>fn</kbd> (such as changing brightness or volume). KMonad seizes
keyboard events before the function-lock feature is
applied. Therefore, under KMonad, this feature will no longer take
effect, but may be imitated in your KMonad configuration.

### `defsrc` block
KMonad translates an input stream of key-events to an output stream of key
events. The `defsrc` block exists as the specification of the input-layout. In
the `(deflayer ...)` blocks, buttons will be mapped to the corresponding input
keycodes from `(defsrc ...)`.
