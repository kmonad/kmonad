module KMonad.Domain.Effect

where

import KMonad.Prelude

import KMonad.Core
import KMonad.Domain.Event



class HasAwaitEvent e where
  awaitEvent :: Lens' e (IO Event)

class HasHandlerFunc e where
  handlerFunc :: Lens' e (KeyEvent -> RIO e ())
