-- FIXME: add description, and other details

module KMonad.App.KeyIO.Mac.Ext
  ( withExt )
where

import KMonad.Prelude

import Foreign.Ptr
import Foreign.Marshal hiding (void)
import Foreign.Storable

import KMonad.App.KeyIO.Common
import KMonad.Util
import KMonad.Util.Keyboard.Mac


-------------------------------------------------------------------------------
-- $ffi

foreign import ccall "send_key"
  send_key :: Ptr RawEvent -> OnlyIO Word8

sendKey :: Ptr RawEvent -> OnlyIO ()
sendKey = void . send_key

-------------------------------------------------------------------------------

withExt :: LUIO m e => ExtCfg -> Ctx r m PutKey
withExt _ = mkCtx $ \f -> do

  let init :: LUIO m e => m (Ptr RawEvent)
      init = do
        logInfo "Initializing Mac key sink"
        liftIO $ mallocBytes $ sizeOf (undefined :: RawEvent)

  let cleanup :: LUIO m e => Ptr RawEvent -> m ()
      cleanup ptr = do
        logInfo "Closing Mac key sink"
        liftIO $ free ptr

  let sendEvent :: IO m => Ptr RawEvent -> KeySwitch -> m ()
      sendEvent ptr e = liftIO $ poke ptr (mkRaw sw $ e^.code) >> sendKey ptr
        where
          sw = if e^.switch == Press then MacPress else MacRelease

  bracket init cleanup $ \ptr -> f (sendEvent ptr)
