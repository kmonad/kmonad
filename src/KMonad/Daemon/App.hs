{-|
Module      : KMonad.Daemon.App
Description : The core daemon functionality of KMonad
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)
-}
module KMonad.Daemon.App
  ( startDaemon
  , HasKApp(..)
  )
where

import KMonad.Prelude

import KMonad.Daemon.Types
import KMonad.Daemon.KeyHandler

-- import KMonad.Domain.Dispatch
-- import KMonad.Domain.KeyHandler
-- import KMonad.Domain.KeyIO
-- import KMonad.Domain.Types

--------------------------------------------------------------------------------

-- | 'KApp' describes the running environment for a KMonad daemon
data DaemonApp = DaemonApp
  { _kaDaemonCfg  :: !DaemonCfg
  , _kaKeySink    :: !KeySink
  , _kaLogFunc    :: !LogFunc
  , _kaDispatch   :: !Dispatch
  , _kaKeyHandler :: !KeyHandler
  }
makeLenses ''DaemonApp

instance HasMapCfg     DaemonApp where mapCfg     = daemonCfg . mapCfg
instance HasRunCfg     DaemonApp where runCfg     = daemonCfg . runCfg
instance HasPort       DaemonApp where port       = daemonCfg . port
instance HasDaemonCfg  DaemonApp where daemonCfg  = kaDaemonCfg
instance HasKeySink    DaemonApp where keySink    = kaKeySink
instance HasDispatch   DaemonApp where dispatch   = kaDispatch
instance HasKeyHandler DaemonApp where keyHandler = kaKeyHandler
instance HasLogFunc    DaemonApp where logFuncL   = kaLogFunc


class (HasDaemonCfg e, HasKeySink e, HasKeySource e) => HasDaemonApp e where
  kApp :: Lens' e DaemonApp

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
      let app = DaemonApp
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

handleEvent Quit =
  pure ()

handleEvent (KIOEvent e) = do
  logInfo $ "Handling key event" <> fromString (show e)
  handleKey e
  step

handleEvent (MessageEvent msg) = do
  logInfo $ "Handling msg event" <> fromString (show msg)
  _ <- undefined
  step
