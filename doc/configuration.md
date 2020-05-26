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

### defcfg block
The `defcfg` block contains some basic settings like how to read events from the
OS and how to send events to the OS. Each configuration option is named, and its
value must follow its fixed name. The minimal complete definition includes an
`input` and an `output` value.

#### Linux
Under Linux, the only valid values are:
- `input`:  `(device-file "/path/to/file")`.
- `output`: `(uinput-sink "Name of keyboard" "post-init command")`
