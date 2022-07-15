<div align="center"> <h1>Configuration</h1> <p><i>kind and clear</i></p> </div>

## Overview
There are 3 different ways by which to configure `kmonad`:
- [invocation](#invocation) &#8594; how you call the `kmonad` command
- [cfg-file](#cfg-file) &#8594; a [`.dhall`](https://github.com/dhall-lang/dhall-lang) configuration file
- [keymap-file](#keymap-file) &#8594; a file in our own [`klang`](#klang) DSL

Many of the settings can be provided in either the invocation or the cfg-file. If a setting is specified in both, then the invocation takes preference. This way you can override our defaults in your dhall file, and you can override your defaults during invocation.

Finally, there are a number of DSLs used throughout the `kmonad` configuration.
- [exprs](#expr) &#8594; various mini languages for `kmonad` [settings](#settings)
- [klang](#klang) &#8594; a DSL for expressing keymaps

### Invocation
`kmonad` can be called from the command-line without any arguments, in which case it chooses [default values](#settings) for many of its settings. Most of the arguments are [settings](#settings) that can also be found in the [dhall file](#dhall-file). The following are unique options that can only be provided via the command-line.
- [`--do`](#do) &#8594; the core task `kmonad` should perform
- `--cfg-file` &#8594; a [path expression](#path) to a [cfg-file](#cfg-file)
- `--version` &#8594; view the version and commit-hash of this executable
- `--help` &#8594; view the built-in documentation


##### `--do`
KMonad currently comes with 3 /modes/. We can:
- `run` &#8594; default; load config, capture keyboard, run remap process
- `cfg-test` &#8594; only try to load the config, for debugging
- `ev-test` &#8594; read and print out the events coming from keyboard input, helpful for figuring out which keys send which signals.

### Settings
Settings are values that are configurable both from the [invocation](#invocation) and the [cfg-file](#cfg-file). In both cases they share the same name, the only differences being the `--`-prefix used in the invocation, and that all values except `True` and `False` are strings in the [cfg-file](#cfg-file) and so need to be wrapped in`""`-marks.

| setting name                    | default value  |
|---------------------------------|----------------|
| [`keymap-file`](#keymap-file)   | `x:keymap.kbd` |
| [`fallthrough`](#fallthrough)   | `False`        |
| [`cmd-allow`](#cmd-allow)       | `no-cmds`      |
| [`log-level`](#log-level)       | `warn`         |
| [`key-repeat`](#key-repeat)     | `no-repeat`    |
| [`input-cfg`](#input-cfg)       | `stdin`        |
| [`output-cfg`](#output-cfg)     | `stdout`       |
| [`pre-kio-cmd`](#pre-kio-cmd)   | `pass`         |
| [`post-kio-cmd`](#post-kio-cmd) | `pass`         |

#### `fallthrough`
The [keymap](#keymap-file) describes a code-to-action mapping. However, not all keycodes need occur in a keymap. `fallthrough` describes what is done with uncaught keys:
- `True` &#8594; emit unaltered from output keyboard
- `False` &#8594; quietly ignore the event

Essentially, if you want `kmonad` to perform a complete remap of your keyboard, you will probably want `fallthrough` set to `False`, to ensure no unwanted keys sneak through. If you intend to use `kmonad` to add functionality to only a subset of your keyboard, use `True`.

#### `cmd-allow`
`kmonad` can be instructed to perform [shell commands](#cmd) at certain opportunities. However, shell commands can be very effectful on a system. This option allows you to not, totally, or partially allow `kmonad` to execute shell calls. We can:
- `"no-cmds"` &#8594; refuse to execute any shell call
- `"init-cmds"`&#8594; only allow [`pre-kio-cmd`](#pre-kio-cmd) & [`post-kio-cmd`](#post-kio-cmd)
- `"all-cmds"` &#8594; allow all shell calls

#### `log-level`
How important a message must be before `kmonad` logs it. Note that each level contains the level above it. For example: `info` includes all `info`, `warn`, and `error` messages.
- `error` &#8594; only sent just before `kmonad` intends to crash 
- `warn` &#8594; something is strange, but we can keep going
- `info` &#8594; updates on major app changes
- `debug` &#8594; updates on minor app changes

#### `key-repeat`
`kmonad` does not represent keyboard repeat events internally. However, we provide 3 different ways to send keyboard events to the OS:
- `no-repeat` &#8594; do not send any repeat events and let the OS figure it out (works for X11 and Mac)
- [`simul:_delay:_rate`](#simul) &#8594; simulate key repeat events
- [`echo`](#echo) &#8594; detect OS repeat events and, if it makes sense, echo them out

##### `simul`
A `key-repeat` setting of `simul:_delay:_rate` instructs `kmonad` to simulate key repeat events. The `_delay` and `_rate` fields are non-negative integers describing a period of time in milliseconds. Here:
- `_delay` &#8594; how long to wait before sending the first key repeat event
- `_rate` &#8594; how long to wait between each following repeat event

For example:
- `simul:300:100` &#8594; wait 300ms, then send a repeat, then send a repeat every 100ms
- `simul:500:10` &#8594; wait 500ms, then send a repeat, then send a repeat every 10ms



A key stops repeating when it is released, or when another key is pressed *on the output keyboard*. `kmonad` will not stop repeating if a key is pressed that does not trigger keyboard output, and it will stop repeating if output is triggered via some other means.

##### `echo`
Not implemented yet, but there is a good idea in: #545

#### `input-cfg`
TODO: Keep going
