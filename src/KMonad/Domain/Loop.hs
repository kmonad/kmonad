{-|
Module      : KMonad.Domain.Loop
Description : The central App-loop of KMonad
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

import Control.Lens
import KMonad.Core
import KMonad.Domain.Effect

-- | Loop over events, pass 'Press' and 'Release' events to the handler while
-- ignoring 'Repeat' events. Break the loop on a 'Quit' event.
loop :: (MonadNext m, MonadTrace m, MonadHandler m) => m ()
loop = trace "Starting event loop" >> loop'
  where loop' = nextEvent >>= \case
          Quit          -> trace "Exiting"
          InputEvent e  -> case e^._type of
            Repeat -> loop'
            _      -> do
              handle e
              loop'
