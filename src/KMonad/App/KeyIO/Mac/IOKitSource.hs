{-|
Module: KMonad.App.KeyIO.Mac.IOKitSource
Description: Use mac c-api to grab keyboard(s).
TODO: complete this
-}

module KMonad.App.KeyIO.Mac.IOKitSource
  ( withIOKitSource )
where

import KMonad.Prelude

import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal hiding (void)
import Foreign.Storable

import KMonad.App.KeyIO.Common
import KMonad.Util
import KMonad.Util.Keyboard.Mac


-------------------------------------------------------------------------------
-- $ffi

-- | Use the mac c-api to grab keyboard(s)
foreign import ccall "grab_kb"
  grab_kb :: CString -> OnlyIO Word8

grabKb :: CString -> OnlyIO ()
grabKb = void . grab_kb

-- | Release the keyboard hook
foreign import ccall "release_kb"
  release_kb :: OnlyIO Word8

releaseKb :: OnlyIO ()
releaseKb = void release_kb

-- | Pass a pointer to a buffer to wait_key, and when it
-- returns, the buffer can be read for the next key event.
foreign import ccall "wait_key"
  wait_key :: Ptr RawEvent -> OnlyIO Word8

waitKey :: Ptr RawEvent -> OnlyIO RawEvent
waitKey ptr = do
       re <- wait_key ptr >> peek ptr
       -- Filter `Keyboard_Reserved` keycode sent on each key event.
       -- E.g. tap a -> P(0x7,0xFFFFFFFF) Pa Ra R(0x7,0xFFFFFFFF)
       case (re^.reCode) of
         (0x7, 0xFFFFFFFF) -> waitKey ptr
         -- (0x7, 0x1)        -> waitKey ptr
         _                 -> return re

-------------------------------------------------------------------------------

withIOKitSource :: LUIO m e => IOKitCfg -> Ctx r m GetKey
withIOKitSource cfg = mkCtx $ \f -> do

  let prodStr :: Maybe Text
      prodStr = cfg^.productStr

  let init :: LUIO m e => m (Ptr RawEvent)
      init = do
        case prodStr of
          Nothing -> do
            logInfo "Opening HID devices"
            liftIO $ grabKb nullPtr
          Just s -> do
            logInfo $ "Opening HID device: " <> s
            liftIO $ withCString (unpack s) grabKb

        liftIO . mallocBytes $ sizeOf (undefined :: RawEvent)

  let cleanup :: LUIO m e => Ptr RawEvent -> m ()
      cleanup ptr = do
        case prodStr of
          Nothing -> logInfo "Closing HID devices"
          Just s -> logInfo $ "Closing HID device: " <> s

        liftIO $ releaseKb >> free ptr

  let nextEvent :: IO m => Ptr RawEvent -> m KeySwitch
      nextEvent ptr = do
        re <- liftIO $ waitKey ptr
        pure $ mkKeySwitch (if re^.reVal == 0 then Release else Press)
                           (Keycode $ re^.reCode)

  bracket init cleanup $ \ptr -> f (nextEvent ptr)
