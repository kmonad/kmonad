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

-}
module KMonad.App.KeyIO.Windows.SendEvent
  ( withSendEvent
  )
where

import KMonad.Prelude

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

import KMonad.App.KeyIO.Common
import KMonad.Util
import KMonad.Util.Keyboard.Windows


--------------------------------------------------------------------------------

foreign import ccall "sendKey" sendKey :: Ptr RawEvent -> OnlyIO ()

-- | The context of an active windows send-event output
withSendEvent :: LUIO m e => SendEventCfg -> Ctx r m PutKey
withSendEvent _ = mkCtx $ \f -> do

  let init = do
        logInfo "Initializing Windows key sink"
        liftIO $ mallocBytes (sizeOf (undefined :: RawEvent))

  let cleanup ptr = do
        logInfo "Closing Windows key sink"
        liftIO . free $ ptr

  bracket init cleanup $ \ptr -> f (skSend ptr)

-- | Write an event to the pointer and prompt windows to inject it
--
-- NOTE: This can throw an error if event-conversion fails.
skSend :: (IO m, HasKeySwitch a) => Ptr RawEvent -> a -> m ()
skSend ptr a = throwIO $ poke ptr (mkRaw sw $ a^.code) >> sendKey ptr where
  sw = if a^.switch == Press then WindowsPress else WindowsRelease
