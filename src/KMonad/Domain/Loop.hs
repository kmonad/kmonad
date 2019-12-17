{-|
Module      : KMonad.Domain.Loop
Description : The central program-loop of KMonad
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)
-}
module KMonad.Domain.Loop
  ( loop
  )
where

import KMonad.Prelude

import KMonad.Domain.Effect
import KMonad.Domain.Event


--------------------------------------------------------------------------------



type CanLoop e = (HasAwaitEvent e, HasHandlerFunc e, HasLogFunc e)

-- | The central app-loop of KMonad
loop :: CanLoop e => RIO e ()
loop = logInfo "Enterring app-loop" >> loop'
  where
    loop' :: CanLoop e => RIO e ()
    loop' =
      do
        env <- ask
        (liftIO $ env^.awaitEvent) >>= \case
          Quit         -> logInfo "Exiting"
          InputEvent e -> logInfo "Handling" >> (env^.handlerFunc $ e) >> loop'
