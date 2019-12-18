module KMonad.Domain.Effect

where

import KMonad.Prelude

import KMonad.Core
import KMonad.Domain.Event



class HasAwaitEvent e where
  awaitEvent :: Lens' e (IO Event)

class HasHandlerFunc e where
  handlerFunc :: Lens' e (KeyEvent -> RIO e ())

handle :: HasHandlerFunc e => KeyEvent -> RIO e ()
handle k = view handlerFunc >>= ($ k)

class HasInjectFunc e where
  injectFunc :: Lens' e (Event -> RIO e ())

inject :: HasInjectFunc e => Event -> RIO e ()
inject e = view injectFunc >>= ($ e)
