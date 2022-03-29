{-|
Module: KMonad.App.KeyIO.Mac.IOKitSource
Description: Use mac c-api to grab keyboard(s).
TODO: complete this
-}

module KMonad.App.KeyIO.Mac.IOKitSource
  ( withIOKitSource )
where

import KMonad.Prelude
import qualified Debug.Trace as D (trace)

import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal hiding (void)
import Foreign.Storable

import KMonad.App.KeyIO.Common
import KMonad.Util
import KMonad.Util.Keyboard.Mac


-------------------------------------------------------------------------------
-- $ffi

-- | DeviceProperties datatype
--
-- It is only used to filter devices when grabbing them.
data DeviceProperties = DeviceProperties
  { _manufacturer :: CString
  , _productName  :: CString
  , _serialNumber :: CString
  , _transport    :: CString
  , _countryCode   :: Word64
  , _locationID    :: Word64
  , _productID     :: Word64
  , _vendorID      :: Word64
  , _versionNumber :: Word64
  } deriving Show
-- makeClassy ''DeviceProperties

-- | For sending struct from Haskell to C.
instance Storable DeviceProperties where
  alignment _ = 8
  sizeOf    _ = 72
  poke ptr (DeviceProperties mf pn sn tt cc lid pid vid vn) = do
    pokeByteOff ptr  0 mf
    pokeByteOff ptr  8 pn
    pokeByteOff ptr 16 sn
    pokeByteOff ptr 24 tt
    pokeByteOff ptr 32 cc
    pokeByteOff ptr 40 lid
    pokeByteOff ptr 48 pid
    pokeByteOff ptr 56 vid
    pokeByteOff ptr 64 vn

  peek ptr = do
    mf  <- peekByteOff ptr  0
    pn  <- peekByteOff ptr  8
    sn  <- peekByteOff ptr 16
    tt  <- peekByteOff ptr 24
    cc  <- peekByteOff ptr 32
    lid <- peekByteOff ptr 40
    pid <- peekByteOff ptr 48
    vid <- peekByteOff ptr 56
    vn  <- peekByteOff ptr 64
    return $ DeviceProperties mf pn sn tt cc lid pid vid vn

-- | Use the mac c-api to grab keyboard(s)
foreign import ccall "grab_kb"
  grab_kb :: Ptr DeviceProperties -> OnlyIO Word8

grabKb :: Ptr DeviceProperties -> OnlyIO ()
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
       -- Filter unwanted keycodes sent on each key event.
       case (re^.reCode) of
         (0x7, 0xFFFFFFFF) -> waitKey ptr
         (0x7, 0x1)        -> waitKey ptr
         _                 -> return re

-------------------------------------------------------------------------------

withIOKitSource :: LUIO m e => (Maybe IOKitCfg) -> Ctx r m GetKey
withIOKitSource maybeCfg = mkCtx $ \f -> do

  let deviceProperties :: IOKitCfg -> OnlyIO (Ptr DeviceProperties)
      deviceProperties cfg = do
        ptr <- calloc

        let makeStr cfg id = newCString $ unpack (cfg^.id)

        mf <- makeStr cfg cfgManufacturer
        pn <- makeStr cfg cfgProductName
        sn <- makeStr cfg cfgSerialNumber
        tt <- makeStr cfg cfgTransport
        poke ptr $ DeviceProperties
                   { _manufacturer = mf
                   , _productName  = pn
                   , _serialNumber = sn
                   , _transport    = tt
                   , _countryCode   = cfg^.cfgCountryCode
                   , _locationID    = cfg^.cfgLocationID
                   , _productID     = cfg^.cfgProductID
                   , _vendorID      = cfg^.cfgVendorID
                   , _versionNumber = cfg^.cfgVersionNumber
                   }
        return ptr

  let init :: LUIO m e => m (Ptr RawEvent)
      init = do
        case (maybeCfg) of
          Nothing -> do
            liftIO $ grabKb nullPtr
          (Just cfg) -> do
            liftIO $ do
              dp <- deviceProperties cfg
              str <- print dp
              let t f dp = (D.trace ("DeviceProperites: \n" <> str) f dp)
              t grabKb dp

        logInfo "Opening HID device(s)"
        liftIO . mallocBytes $ sizeOf (undefined :: RawEvent)

  let cleanup :: LUIO m e => Ptr RawEvent -> m ()
      cleanup ptr = do
        logInfo "Closing HID device(s)"
        liftIO $ releaseKb >> free ptr

  let nextEvent :: IO m => Ptr RawEvent -> m KeySwitch
      nextEvent ptr = do
        re <- liftIO $ waitKey ptr
        pure $ mkKeySwitch (if re^.reVal == 0 then Release else Press)
                           (Keycode $ re^.reCode)

  bracket init cleanup $ \ptr -> f (nextEvent ptr)

  where
    print dp = do
      dp <- peek dp
      let get x dp = peekCString (x dp)
      m <- get _manufacturer  dp
      p <- get _productName   dp
      s <- get _serialNumber  dp
      t <- get _transport     dp
      return $ "manu: " <> m <> " :: " <> show (length m) <> "\n" <>
               "name: " <> p <> " :: " <> show (length p) <> "\n" <>
               "seri: " <> s <> " :: " <> show (length s) <> "\n" <>
               "tran: " <> t <> " :: " <> show (length t) <> "\n" <>
               "coun: " <> show (_countryCode   dp) <> "\n" <>
               "loca: " <> show (_locationID    dp) <> "\n" <>
               "prid: " <> show (_productID     dp) <> "\n" <>
               "vend: " <> show (_vendorID      dp) <> "\n" <>
               "vrsn: " <> show (_versionNumber dp) <> "\n"
