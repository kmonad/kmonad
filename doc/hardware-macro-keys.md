# Hardware Macro Keys

Some keyboards have hardware macro keys.
They are especially common with laptops shipping with some advertised Microsoft features.

Some of those keys include:

| Key                    | Macro                                  | Further References |
|------------------------|----------------------------------------|--------------------|
| [Copilot Key][copilot] | `Plmet Plsft Pf23`                     | #931               |
| [Office Key][office]   | `Plmet Plalt Plctl Plsft` (unverified) |                    |

[copilot]: https://www.microsoft.com/en-us/windows/learning-center/unlock-productivity-with-the-copilot-key
[office]: https://support.microsoft.com/en-us/office/using-the-office-key-df8665d3-761b-4a16-84b8-2cfb830e6aff

## Remapping

There are multiple methods of remapping such hardware macro keys:

- [Mapping a primary key](#mapping-a-primary-key).
  The other key events are kept, and we need a "primary" (e.g. F23 for Copilot).
- [Filtering out non-primary modifiers](#filtering-out-non-primary-modifiers).
  This fixes the issue of the other key events being kept but still needs a "primary".
- [Multi-layer solution](#multi-layer-solution).
  Slightly improves accuracy. Wanted if we don't have a "primary" key (e.g. with the Office key).
- [Filtered multi-layer solution](#filtered-multi-layer-solution).
  Same improvements as [Filtering out non-primary modifiers](#filtering-out-non-primary-modifiers)
  but without the need for a "primary" key.

There are further improvements to the detection possible
which would increase the complexity in KMonad too much
or are not worth implementing. If you want to try yourself
against this problem, there are some improvement ideas
collected in the [Further Improvement Ideas](#further-improvement-ideas)
section.

If you have improved on the remapping possibilities found
in this document (either via a different config setup or
via an external program), please open an issue / pr to
include it here.

### Mapping a primary key

This is quite simple.

If you don't care about presses of the other events and can safely
map an entire key code from the sequence which can't
be pressed any other way, it can simply be done by mapping said key.

For the Copilot key, it would look like the following:

```clojure
(defsrc f23)
(deflayer base
  #(c o p i l o t)
)
```

Though if you also want to map `lmet` or `lsft` without it being triggered by `copilot`,
or don't want to open some kind of menu when pressing `lmet`, you might want to see the next option.

### Filtering out non-primary modifiers

For the Copilot key (according to the testing of #931), there is a delay of 0.3ms between every key event.
So a button like `(tap-hold-next 1 XX XX :timeout lmet)` should safely drop the `lmet` if it is followed
by another key within one millisecond and let it through otherwise.

In general, if we have a config like this:

```clojure
(defsrc lsft lmet)
(deflayer base
  #(d o n ' t)
  #(p r e s s spc m e)
)
```

and want to map `copilot` we can change it to the following:

```clojure
(defsrc lsft lmet f23)
(deflayer base
  (tap-hold-next 1 XX XX :timeout #(d o n ' t))
  (tap-hold-next 1 XX XX :timeout #(p r e s s spc m e))
  #(c o p i l o t)
)
```

Technically `(tap-hold-next-press 1 lmet XX :timeout lmet)` would be more accurate since
we catch some more corner cases which are not Copilot key sequences.

Via personal testing, I could only get to ~2ms of difference between two key presses (on a laptop)
when trying to press keys simultaneously. With more load or worse scheduling, this could conceivably
be in range of causing issues.

### Multi-layer solution

Since the key sequence is fixed, we can define layers to detect if we are inside such a sequence.
For the unconfirmed office key sequence of `Plmet Plalt Plctl Plsft`, a mapping such as the one below is conceivable:

```clojure
(defsrc lmet lalt lctl lsft)
(deflayer base (layer-toggle lmet) _ _ _)
(deflayer lmet _ (layer-toggle lmet-lalt) _ _)
(deflayer lmet-lalt _ _ (layer-toggle lmet-lalt-lctl) _)
(deflayer lmet-lalt-lctl _ _ _ #(o f f i c e))
```

Using named `defsrc` blocks can also help a lot if other keys are also remapped.

### Filtered multi-layer solution

The solutions from [Filtering out non-primary modifiers](#filtering-out-non-primary-modifiers)
and [Multi-layer solution](#multi-layer-solution) can be combined to a config allowing
mapping the modifier keys while also using layers to better determine the key:

```clojure
(defsrc lmet lalt lctl lsft)
(deflayer base (tap-hold-next 1 XX (layer-toggle lmet) :timeout lmet) _ _ _)
...
```

## Further Improvement Ideas

Here we outline some ideas one could implement in an external software
to better support remapping keys.

### Differentiating keys after `tap-hold-next-press`

(Using `copilot` as an example)

The `tap-hold-next-press` approach from above drops the `lmet` independently of
whatever key comes after. It might be that under heavy load / bad scheduling
the delay between key events cannot be differentiated from a simultaneous press of
the user (personal testing reached ~2ms of delay). Only dropping the `lmet` event
if the next event is indeed an `lsft` (or `f23` if `lsft` has been pressed before)
could further reduce incorrect behaviour.

### Guessing presses of modifiers while keys are held

It seems the key repeat events differ for the `lsft` pressed by
`copilot` and by the user. This might be the case for other keys
(like the Office key) too.

In the particular case of [#931](https://github.com/kmonad/kmonad/issues/931),
the Copilot key does not emit any key repeat events. `lsft` and `lmet`
do emit them after 300ms. So if they start while `f23` is pressed we can
determine that `lsft` / `lmet` (depending on the repeat event) was
pressed 300ms prior.

Logs of this can be found in a [reply in #931 by @AlejandroMinaya](https://github.com/kmonad/kmonad/issues/931#issuecomment-4128757705).
