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
  , _repEnv :: RepeatEnv      -- ^ Context to manage key-repeat
  , _ptr    :: Ptr RawEvent   -- ^ Pointer used to communicate with Windows
  }
makeClassy ''SendEventEnv

instance HasSendEventCfg SendEventEnv where sendEventCfg = seCfg
instance HasRepeatEnv    SendEventEnv where repeatEnv    = repEnv

type SIO a = RIO SendEventEnv a

-- | Send a key-event to Windows
sendKey :: EvType -> Keycode -> SIO ()
sendKey t c = do
  p <- view ptr
  liftIO $ do
    poke p (mkRaw t c)
    c_sendKey p

--------------------------------------------------------------------------------

-- | Handle any key event we are trying to send to the OS
handleEvent :: KeySwitch -> SIO ()
handleEvent (KeySwitch Press c)   = sendKey WindowsPress c   >> startRepeat c
handleEvent (KeySwitch Release c) = sendKey WindowsRelease c >> stopRepeat c

--------------------------------------------------------------------------------

-- | The context of an active windows send-event output
withSendEvent :: LUIO m e => SendEventCfg -> Ctx r m PutKey
withSendEvent c = mkCtx $ \f -> do

  let init = do
        logInfo "Initializing Windows key sink"
        SendEventEnv c
          <$> newMVar M.empty
          <*> liftIO (mallocBytes (sizeOf (undefined :: RawEvent)))

  let cleanup e = do
        logInfo "Closing Windows key sink"
        pressed <- M.keys <$> (readMVar $ e^.prcs)
        runRIO e $ do
          mapM_ handleRelease pressed
          mapM_ (sendKey WindowsRelease) pressed
        liftIO . free $ e^.ptr

  bracket init cleanup $ \env -> f (\e -> runRIO env $ handleEvent e)

-- -- | Write an event to the pointer and prompt windows to inject it
-- --
-- -- NOTE: This can throw an error if event-conversion fails.
-- skSend :: (IO m, HasKeySwitch a) => Ptr RawEvent -> a -> m ()
-- skSend ptr a = do
--   let c = a^.keySwitch.code
--   let s = if a^.keySwitch.switch == Press then WindowsPress else WindowsRelease
--   liftIO $ do
--     poke ptr (mkRaw s c)
--     sendKey ptr
