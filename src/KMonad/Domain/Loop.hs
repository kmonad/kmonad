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
        (liftIO $ env^.awaitEvent) >>= handleEvent


--------------------------------------------------------------------------------

-- | Different handlers for all the different events
handleEvent :: (HasHandlerFunc e, HasLogFunc e) => Event -> RIO e ()

handleEvent Quit = do
  logInfo "Exiting"
  pure ()

handleEvent (InputEvent e) = do
  logInfo $ "Handling key event" <> fromString (show e)
  handle e

handleEvent (MessageEvent msg) = do
  logInfo $ "Handling msg event" <> fromString (show msg)
  undefined
