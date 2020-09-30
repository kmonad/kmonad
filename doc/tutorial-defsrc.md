# Tutorial: Understanding defsrc and deflayer

In this tutorial we'll create tiny `defsrc` and `deflayer` configurations to get a really solid understanding of how these work.

## Prerequisites: Working kmonad and defcfg

Please ensure you have kmonad installed and ready to run and you have figured out a `(defcfg)` expression in your config that works properly with your keyboard. This tutorial is going to focus exclusively on the `(defsrc)` and `(deflayer)` sections of your `.kbd` configuration file.

We'll show a complete `.kbd` file but you should understand that the example is from a linux system. Many settings within `defcfg` will be different depending on your specific keyboard and OS.

## Remapping a single key

- Create a file `learn.kbd` similar to the one below.

```
(defcfg
  input  (device-file "/dev/input/by-path/platform-i8042-serio-0-event-kbd")
  output (uinput-sink "kmonad")
  fallthrough true
  allow-cmd false
)

(defsrc 1) (deflayer learn k)
```
- Start kmonad with: `kmonad learn.kbd`
- Type your keyboard's `1` key

You should see a letter `k` echoed by your terminal if you are still in the terminal with kmonad running, which is fine for the purposes of this tutorial. You can also switch to a text editor and you should see the same behavior.

So now we understand a key thing: **Your defsrc does not need to represent every key on your keyboard**. In fact, it can be as simple as a single key. If all you want is to make your CapsLock key work as control, you can just configure that. It'll work on pretty much every keyboard, and you're done.

We also now know how to do basic key remapping. Put an input key symbol in your `defsrc` and at the same positition (order) in your `deflayer` put the corresponding output key symbol.

We also should now understand that the first `deflayer` entry in the file is enabled when kmonad starts.

## Understanding fallthrough

With kmonad still running, type other keys on your keyboard like letters, other numbers, etc. You should see them echoed in the terimal as you would expect. This is what is meant by `fallthrough true` in your `defcfg`. It just means kmonad will pass through unmodified anything else it sees from the input keyboard.

One more thing to try: Hold `shift` as a modifier and type `1`. Do you get `!`, `K`, or something else? You should see `K` which is just another consequence of `fallthrough true`. kmonad passes through the shift key presses and when you press `1` with `shift` held kmonad sends `K`.

## Remapping several keys in "order"

Exit kmonad with `control+c`, change your `learn.kbd` to look as below and try that out.

```
(defcfg
  input  (device-file "/dev/input/by-path/platform-i8042-serio-0-event-kbd")
  output (uinput-sink "kmonad")
  fallthrough true
  allow-cmd false
)

(defsrc 1 2 3 4 5 6) (deflayer learn k m o n a d)
```

Now type `1 2 3 4 5 6`. You should see `k m o n a d`. You can remap as many keys as you like.

Now let's change the order to solidify our understanding.

```
(defcfg
  input  (device-file "/dev/input/by-path/platform-i8042-serio-0-event-kbd")
  output (uinput-sink "kmonad")
  fallthrough true
  allow-cmd false
)

(defsrc 2 4 1 6 5 3) (deflayer learn m n k d a o)
```

Now type `1 2 3 4 5 6` again. You'll again see `k m o n a d`. What we want to understand is **the order of keys in `defsrc` does not have to correspond to your keyboard's physical key layout**. You may see lots of examples with `defsrc` laid out in an ASCII art portrait of a keyboard. That's fine and some may find that the clearest way, but kmonad doesn't require that. You can model your input logically and thereby make it easier to maintain and more portable across keyboards.

For example, some users like to lay out their `defsrc` into sections like:

```
;; letters
q
w
e
r
;; ...and so on

;; modifiers
caps
lsft
lctl
lmet
lat
```

Now the ordering **does** have to match up exactly between `defsrc` and `deflayer` but other than that, anything goes.

So to make it easy to track between the two sections, you can set up your `deflayer` with one key per line and an end-of-line comment reminder of which line from `defsrc` corresponds. So for example in a "dvorak" layer, the home row looks like this:

```
(deflayer dvorak

  ;; some lines omitted here for clarity

  a ;; a
  o ;; s
  e ;; d
  u ;; f
  i ;; g
  d ;; h
  h ;; j
  t ;; k
  n ;; l
  s ;; ;
  - ;; '
   
  ;; some lines omitted here for clarity
)
```

First is the layer output key and after the 2-semicolon line comment is the input `defsrc` corresponding key.

## Use small files to learn fancier buttons

If this tutorial's approach worked well for you, consider using a similar approach as you experiment with kmonad's more complex button features like `tap-hold` and friends.
