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

import Control.Monad.Logger
import KMonad.Core
import KMonad.Domain.Effect

-- | Loop over events, pass 'Press' and 'Release' events to the handler while
-- ignoring 'Repeat' events. Break the loop on a 'Quit' event.
loop :: (MonadNext m, MonadHandler m, MonadLogger m) => m ()
loop = $(logInfo) "Starting event loop" >> loop'
  where loop' = nextEvent >>= \case
          Quit          -> $(logInfo)  "Exiting"
          InputEvent e  -> $(logDebug) "Handling" >> handle e >> loop'
