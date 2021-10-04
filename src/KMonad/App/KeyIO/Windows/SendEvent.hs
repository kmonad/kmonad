{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-|
Module      : KMonad.App.KeyIO.Windows.SendEvent
Description : Using Windows' send_event functionality to inject KeyEvent's
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

This uses @sendKey@ from the @keyio_win.c@ to send keys to Windows. This itself
then uses the Windows 'SendInput' system call.

Since we filter out everything from KMonad's core except for alternating
press-release events, this breaks Window's key-repeat. We manually reintroduce
keyrepeat in this module.

-}
module KMonad.App.KeyIO.Windows.SendEvent
  ( withSendEvent
  )
where

import KMonad.Prelude

import Foreign.Ptr
import Foreign.Marshal hiding (void)
import Foreign.Storable

import KMonad.App.KeyIO.Common
import KMonad.Util
import KMonad.Util.Keyboard.Windows hiding (Keycode)

import qualified RIO.HashMap as M

--------------------------------------------------------------------------------

foreign import ccall "sendKey" c_sendKey :: Ptr RawEvent -> OnlyIO ()

--------------------------------------------------------------------------------

-- | The environment used to handle output operations
data SendEventEnv = SendEventEnv
  { _seCfg  :: SendEventCfg   -- ^ The sink configuration
  , _le     :: LogEnv         -- ^ Reference to the logging environment
  , _ptr    :: Ptr RawEvent   -- ^ Pointer used to communicate with Windows
  , _repEnv :: RepeatEnv      -- ^ Context to manage key-repeat
  }
makeClassy ''SendEventEnv

instance HasSendEventCfg SendEventEnv where sendEventCfg = seCfg
instance HasRepeatEnv    SendEventEnv where repeatEnv    = repEnv
instance HasLogEnv       SendEventEnv where logEnv       = le
instance HasLogCfg       SendEventEnv where logCfg       = logEnv.logCfg


type S a = RIO SendEventEnv a

--------------------------------------------------------------------------------


-- | Send a key-event to Windows
sendKey :: EvType -> Keycode -> S ()
sendKey t c = do
  p <- view ptr
  liftIO $ do
    poke p (mkRaw t c)
    c_sendKey p

-- | Handle any key event we are trying to send to the OS
handleEvent :: HasKeySwitch e => e -> S ()
handleEvent e = let s = e^.keySwitch in if isPress s
  then sendKey WindowsPress   (s^.code)  >> handleRepeat s
  else sendKey WindowsRelease (s^.code)  >> handleRepeat s

--------------------------------------------------------------------------------

-- | The context of an active windows send-event output
withSendEvent :: LUIO m e => SendEventCfg -> Ctx r m PutKey
withSendEvent cfg = mkCtx $ \f -> do

  let init = do

        logInfo "Initializing Windows key sink"
        le <- view logEnv
        p  <- liftIO $ mallocBytes $ sizeOf (undefined :: RawEvent)

        -- Create key-repeat with a partially undefined SendEventEnv
        let env = SendEventEnv cfg le p undefined
        u   <- askRunInIO
        rep <- mkRepeatEnv (cfg^.keyRepeatCfg)
          $ u . runRIO env . sendKey WindowsPress

        pure $ env { _repEnv = rep }

  let cleanup e = do
        logInfo "Closing Windows key sink"
        liftIO . free $ e^.ptr

  bracket init cleanup $ \env -> f $ runRIO env . handleEvent

