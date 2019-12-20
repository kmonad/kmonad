{-|
Module      : KMonad.Domain.Daemon
Description : The core daemon functionality of KMonad
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)
-}
module KMonad.Domain.Daemon
  ( startDaemon
  , HasKApp(..)
  )
where

import KMonad.Prelude

import KMonad.Domain.Dispatch
import KMonad.Domain.KeyHandler
import KMonad.Domain.KeyIO
import KMonad.Domain.Types

--------------------------------------------------------------------------------

-- | 'KApp' describes the running environment for a KMonad daemon
data KApp = KApp
  { _kaDaemonCfg  :: !DaemonCfg
  , _kaKeySink    :: !KeySink
  , _kaLogFunc    :: !LogFunc
  , _kaDispatch   :: !Dispatch
  , _kaKeyHandler :: !KeyHandler
  }
makeLenses ''KApp

instance HasMapCfg     KApp where mapCfg     = daemonCfg . mapCfg
instance HasRunCfg     KApp where runCfg     = daemonCfg . runCfg
instance HasPort       KApp where port       = daemonCfg . port
instance HasDaemonCfg  KApp where daemonCfg  = kaDaemonCfg
instance HasKeySink    KApp where keySink    = kaKeySink
instance HasDispatch   KApp where dispatch   = kaDispatch
instance HasKeyHandler KApp where keyHandler = kaKeyHandler
instance HasLogFunc    KApp where logFuncL   = kaLogFunc


class (HasDaemonCfg e, HasKeySink e, HasKeySource e) => HasKApp e where
  kApp :: Lens' e KApp

--------------------------------------------------------------------------------

type CanLoop e = (HasDispatch e, HasKeyHandler e, HasLogFunc e, HasKeySink e)


startDaemon :: HasLogFunc e => DaemonCfg -> RIO e ()
startDaemon dcfg =
  with (dcfg^.openKeySource) $ \src ->
    with (dcfg^.openKeySink) $ \snk -> do

      -- Initialize all the various moving parts of KMonad
      kmap       <- loadKeyMap $ dcfg^.mapCfg
      logfunc    <- view logFuncL
      dispatch'  <- mkDispatch src
      keyhandler <- mkKeyHandler kmap

      -- Gather everything into a runtime environment
      let app = KApp
            { _kaDaemonCfg  = dcfg
            , _kaKeySink    = snk
            , _kaLogFunc    = logfunc
            , _kaDispatch   = dispatch'
            , _kaKeyHandler = keyhandler
            }

      -- Kick of the KMonad loop
      logInfo "Entering daemon app-loop"
      runRIO app step
      logInfo "Exiting daemon app-loop"

-- | The central app-loop of KMonad. Every action in 'handleEvent' is
-- responsible for calling 'handleEvent' itself to maintain recursion.
step :: CanLoop e => RIO e ()
step = awaitEvent >>= handleEvent

--------------------------------------------------------------------------------

-- | Different handlers for all the different events
handleEvent :: CanLoop e => Event -> RIO e ()

handleEvent Quit = do
  logInfo "Shutting down daemon"

handleEvent (KIOEvent e) = do
  logInfo $ "Handling key event" <> fromString (show e)
  handleKey e
  step

handleEvent (MessageEvent msg) = do
  logInfo $ "Handling msg event" <> fromString (show msg)
  _ <- undefined
  step
