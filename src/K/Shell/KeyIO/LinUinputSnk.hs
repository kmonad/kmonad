-- |
{-# LANGUAGE CPP #-}

module K.Shell.KeyIO.LinUinputSnk where

import K.Shell.KeyIO.Initial

import Foreign.C.String
import Foreign.C.Types

import qualified Control.Exception.Lens as Exc

#if defined linux_HOST_OS
import System.Posix
#endif

-- error -----------------------------------------------------------------------

-- | Things that can go wrong with Uinput devices
data UinputError
  = UinputCreateError Name   -- ^ Failed to register uinput device with kernel
  | UinputReleaseError  Name -- ^ Failed to unregister uinput device with kernel
makeClassyPrisms ''UinputError

instance Show UinputError where
  show (UinputCreateError n)
    = "Could not create uinput device: " <> unpack n
  show (UinputReleaseError n)
    = "Could not release uinput device: " <> unpack n

instance Exception UinputError
instance AsUinputError SomeException where _UinputError = _SomeException

-- ffi -------------------------------------------------------------------------

#if defined linux_HOST_OS

foreign import ccall "acquire_uinput_keysink"
  c_acquire_uinput_keysink
    :: CInt    -- ^ Posix handle to the file to open
    -> CString -- ^ Name to give to the keyboard
    -> CInt    -- ^ Vendor ID
    -> CInt    -- ^ Product ID
    -> CInt    -- ^ Version ID
    -> IO Int

foreign import ccall "release_uinput_keysink"
  c_release_uinput_keysink :: CInt -> IO Int

foreign import ccall "send_event"
  c_send_event :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO Int

#else

msg :: Text
msg =  "LinUinputSnk only works on Linux"

c_acquire_uinput_keysink :: CInt -> CString -> CInt -> CInt -> CInt -> IO Int
c_acquire_uinput_keysink = excThrowing _OSError (Linux, msg)

c_release_uinput_keysink :: CInt -> IO Int
c_release_uinput_keysink = excThrowing _OSError (Linux, msg)

c_send_event :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO Int
c_send_event = excThrowing _OSError (Linux, msg)

#endif

-- basic types -----------------------------------------------------------------

data UinputEnv = UinputEnv
  { _uname :: Name    -- ^ The name used to register device with OS
  , _le    :: LogEnv  -- ^ Reference to the logging environment for ease of use
  , _fdvar :: MVar Fd -- ^ Open uinput file in MVar (ensures only 1 writer at a time)
  }
makeClassy ''UinputEnv

instance HasName UinputEnv where name = uname
instance HasLogEnv UinputEnv where logEnv = le

-- env -------------------------------------------------------------------------

-- | Run an action in the context of an acquired uinput device
withLinUinputSnk :: (UIO m, CanLog env m) => Maybe Name -> (KeySnk -> m a) -> m a
withLinUinputSnk n f = bracket (getUinput n) (inRIO relUinput) $ \env -> do
  f $ KeySnk (runRIO env . writeUinput)

-- | Open a handle to /dev/uinput and register a uinput device with the kernel
getUinput :: (UIO m, CanLog env m) => Maybe Name -> m UinputEnv
getUinput mn = do
  let n = fromMaybe "kmonad simulated keyboard" mn
  le <- view logEnv
  fd <- liftIO . openFd "/dev/uinput" WriteOnly Nothing $
    OpenFileFlags False False False True False
  env <- UinputEnv n le <$> newMVar fd

  logInfo $ "Registering uinput device: " <> n
  cstr <- liftIO . newCString $ unpack n
  let (Fd h) = fd
  -- 0x1234 0x5678 and 0x0000 are vendor-, product- and version- codes respectively
  -- As far as I can tell they are meaningless for our purposes
  liftIO (c_acquire_uinput_keysink h cstr 0x1234 0x5678 0x0000)
    `ffiErr` excThrowing _UinputCreateError n
    `onException` liftIO (closeFd fd)
  pure env

-- | Unregister the uinput device and close the handle
relUinput :: RIO UinputEnv ()
relUinput = withFd $ \fd@(Fd h) -> do
  n <- view name
  let rel = do logInfo $ "Unregistering uinput device: " <> n
               liftIO (c_release_uinput_keysink h)
                 `ffiErr` excThrowing _UinputReleaseError n
  let cls = do logInfo "Closing uinput device-file."
               liftIO $ closeFd fd
  rel `finally` cls

-- ops -------------------------------------------------------------------------

-- | Helper to run a function on the Fd in the MVar in our UinputEnv
withFd :: (Fd -> RIO UinputEnv a) -> RIO UinputEnv a
withFd f = view fdvar >>= \v -> withMVar v $ \fd -> f fd

-- | Write a linux event tuple to the simulated keyboard
sendLinEvent :: LinEvent -> RIO UinputEnv ()
sendLinEvent (LinEvent (_, _, t, c, v)) = withFd $ \(Fd h) -> do
  -- CAREFUL: argument-order to c_send_event is *different* than tuple
  -- NOTE: we don't bother setting the time on injected events, not required
  liftIO . void $ c_send_event h (fi t) (fi c) (fi v) 0 0

-- | Write a 'KioEvent' to the OS as a linux event, then send a sync event
writeUinput :: KioEvent -> RIO UinputEnv ()
writeUinput e = do
  sendLinEvent $ _LinEvent # e -- The actual event encoded to linux form
  sendLinEvent syncEvent       -- Linux event instructing kernel to update state
