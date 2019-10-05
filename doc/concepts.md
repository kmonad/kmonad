# Concepts

The following documentation contains a brief explanation of the concepts behind
KMonad, so that you can get a better idea of what is, and what isn't possible
with KMonad.

## Where does KMonad 'sit'

KMonad attempts to capture keyboard input as close to the raw input-source as
possible. In Linux, this currently means we read input-events from the
`/dev/input` files in binary. The same holds true for KMonad output-events: we
try to inject events as close to the kernel as possible. In Linux this currently
means using the `uinput` subsystem.

This means that the only type of input events we can match on consist of nothing
more than an indication of Press/Release/Repeat and a KeyCode. At that level of
operation, there is no such thing as a special symbol, like `é`. Additionally,
this means we cannot *output* anything more complicated than basic manipulations
of core keycodes either. 

Furthermore, we currently filter out *all* Repeat events at the input, and
provide no means of sending Repeat events to the output. If you want to change
your repeat-delay or repeat-speed, you can use your OS'es methods for adjusting
those values, and that should automatically affect the behavior of KMonad's keys
as well.

Additionally, the *only* input we collect from the OS is the input-stream of
keypresses. We have no access to Window-Manager information, like what
application is currently active, so have no direct way of implementing
application-specific keybindings.

There are plans in the future to provide a mechanism whereby you can manipulate
KMonad's layer-stack using terminal commands to communicate with it in a
client/demon style paradigm. That way it should be possible to track your
window-manager information yourself, and toggle layers automatically, but
support for this does not yet exist.

Note that the fact that KMonad tries to sit so close to the kernel means that it
does not need X11 to function.

## How do we manage layers?

KMonad operates on a conceptually very simple model. Input-events are either a
press, release, or repeat event, and have a corresponding KeyCode. We
immediately filter out all repeat events.

Central to KMonad is the concept of a Layer of Buttons. A Button is a piece of
functionality that can be manipulated in two ways: It can be pressed or
released. Pressing a button twice in a row is the same as pressing it once, and
the same holds for releases. A Layer is simply a mapping of input-keycodes to
buttons.

KMonad can track any number of layers, and maintains an internal stack of
layers. When we register a Push event, then we look at the top-most layer of the
stack, and see if it contains a button matching the Push-event's keycode. If it
does, then that Button is send a Push signal, and whatever action it contains is
executed. If the top layer does not contain a button corresponding to the
input-event's KeyCode, we simply look at the next layer, and the next, until we
either find a Button and perform its action, or we reach the bottom of the stack
and simply do nothing.

When we register a Release event, the stack is not consulted at all. Instead we
keep track of exactly which Button was used to handle a push-event for the
corresponding keycode, and then use that same Button to handle the
release-event.
 
You can (currently) only add layers to the top of the stack, and remove the
first occurence of a layer from the top of the stack.
