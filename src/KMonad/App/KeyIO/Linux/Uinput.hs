module KMonad.App.KeyIO.Linux.Uinput
  ( withUinput )
where

import KMonad.Prelude

-- FIXME: Cleanup old code, then reorganize imports
import KMonad.Util.Ctx
import KMonad.Util.Keyboard
import KMonad.Util.Keyboard.Linux
import KMonad.Util.FFI
import KMonad.Util.Name
import KMonad.Util.Time
import KMonad.Util.Logging
import KMonad.App.KeyIO.Common

import Foreign.C.String
import Foreign.C.Types
import System.Posix
import UnliftIO.Process (spawnCommand)

import Data.Typeable
import UnliftIO.Exception hiding (throwIO)
import System.IO.Error.Lens

--------------------------------------------------------------------------------
-- $cfg

-- | All the things that can go wrong
data UinputException
  = UinputCouldNotCreate  UinputCfg FailCode  -- ^ Could not create uinput device
  | UinputCouldNotDestroy UinputCfg FailCode  -- ^ Could not destroy uinput device
  deriving Show
makeClassyPrisms ''UinputException

instance Exception UinputException
instance AsUinputException SomeException where _UinputException = exception

-- | The environment used to handle uinput operations
data UinputEnv = UinputEnv
  { _cfg     :: UinputCfg -- ^ The configuration with which we were started
  , _le      :: LogEnv    -- ^ Keep a reference to the logging environment
  , _kbf     :: Fd        -- ^ Open file-descriptor to the uinput keyboard
  , _repEnv  :: RepeatEnv -- ^ Reference to the environment used for key-repeat
  }
makeClassy ''UinputEnv

instance HasUinputCfg UinputEnv where uinputCfg = cfg
instance HasRepeatEnv UinputEnv where repeatEnv = repEnv
instance HasLogEnv    UinputEnv where logEnv = le
instance HasLogCfg    UinputEnv where logCfg = le.logCfg

-- | Type shorthand
type U a = RIO UinputEnv a



--------------------------------------------------------------------------------
-- FFI calls and type-friendly wrappers

foreign import ccall "acquire_uinput_keysink"
  c_acquire_uinput_keysink
    :: CInt    -- ^ Posix handle to the file to open
    -> CString -- ^ Name to give to the keyboard
    -> CInt    -- ^ Vendor ID
    -> CInt    -- ^ Product ID
    -> CInt    -- ^ Version ID
    -> OnlyIO CInt

foreign import ccall "release_uinput_keysink"
  c_release_uinput_keysink :: CInt -> OnlyIO CInt

foreign import ccall "send_event"
  c_send_event :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> OnlyIO CInt

-- | Create and acquire a Uinput device
acquireUinputKeysink :: MonadIO m => Fd -> UinputCfg -> m FFIResult
acquireUinputKeysink (Fd h) c = liftIO $ do
  cstr <- newCString $ unpack $ c^.keyboardName
  ffiReturn <$> c_acquire_uinput_keysink h cstr
    (fi $ c^.vendorCode) (fi $ c^.productCode) (fi $ c^.productVersion)

-- | Release a Uinput device
releaseUinputKeysink :: MonadIO m => Fd -> m FFIResult
releaseUinputKeysink (Fd h) = liftIO $ ffiReturn <$> c_release_uinput_keysink h

-- | Using a Uinput device, send a RawEvent to the Linux kernel
send_event ::
     Fd
  -> RawEvent
  -> RIO e ()
send_event (Fd h) e = liftIO $ void $ c_send_event h
  (fi $ e^.leType) (fi $ e^.leCode) (fi $ e^.leVal) (fi $ e^.leS) (fi $ e^.leNS)

--------------------------------------------------------------------------------

-- | Send a linux key-event to the OS
sendEvent :: EvType -> Keycode -> U ()
sendEvent s c = do
  logDebug $ "Putting: " <> dsp s <> " " <> dsp c
  h <- view kbf

  -- Make and send the actual event
  e <- mkRaw s c
  send_event h e

  -- Make and send a sync event to trigger a linux keyboard driver update
  e <- mkSync
  send_event h e

-- | Handle requests for presses and releases
handleEvent :: KeySwitch -> U ()
handleEvent s = dispatch s >> handleRepeat s where
  dispatch s = sendEvent (_SwitchEv # (s^.switch)) (s^.code)


--------------------------------------------------------------------------------

-- | How to manage the context of having a Uinput keyboard
withUinput :: (LUIO m env)
  => UinputCfg -> Ctx r m PutKey
withUinput cfg = mkCtx $ \f -> do

  -- Run our maybe-command if specified
  traverse_ spawnCommand $ cfg^.preInit

  let init = do
        -- Get a reference to the logging env
        le <- view logEnv

        -- Open the file-handle to the standard uinput device
        fd <- liftIO $ openFd "/dev/uinput" WriteOnly Nothing $
            OpenFileFlags False False False True False

        -- Register the device-specs with the uinput kernel module
        logInfo "Registering uinput device"
        acquireUinputKeysink fd cfg `onErr` \n
          -> throwing _UinputCouldNotCreate (cfg, n)

        -- Optionally, fork of a command to be run
        for_ (cfg^.postInit) $ \cmd -> do
          logInfo $ "Running post-uinput-init command: " <> pack cmd
          async . spawnCommand $ cmd

        -- Create key-repeat with a partially undefined UinputEnv
        let env = UinputEnv cfg le fd undefined
        u <- askRunInIO
        rep <- case cfg^.mayRepeatCfg of
          Just repCfg -> mkRepeatEnv repCfg $ u . runRIO env . sendEvent LinuxRepeat
          Nothing     -> mkRepeatEnv def    $ const $ pure ()

        -- Insert key-repeater into env to deliver fully packaged environment
        pure $ env { _repEnv = rep }


  let cleanup env = do
        -- Unregister the device from the uinput kernel module
        logInfo "Unregistering uinput device"
        let h = env^.kbf
        let rel = releaseUinputKeysink h `onErr` \n ->
              throwing _UinputCouldNotDestroy (cfg, n)
        let cls = liftIO $ closeFd h
        finally rel cls

  bracket init cleanup $ \env -> f (runRIO env . handleEvent)
