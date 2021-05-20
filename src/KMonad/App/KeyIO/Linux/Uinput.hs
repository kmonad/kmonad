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
import UnliftIO.Process (callCommand)

import Data.Typeable
import UnliftIO.Exception hiding (throwIO)
import System.IO.Error.Lens



import qualified RIO.Set as S

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
  { _cfg     :: UinputCfg            -- ^ The configuration with which we were started
  , _le      :: LogEnv               -- ^ The logging-env in which we were started
  , _kbf     :: Fd                   -- ^ Open file-descriptor to the uinput keyboard
  , _pressed :: MVar (S.Set Keycode) -- ^ Set of keys that are currently held
  }
makeClassy ''UinputEnv

instance HasUinputCfg UinputEnv where uinputCfg = cfg

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
acquire_uinput_keysink :: MonadIO m => Fd -> UinputCfg -> m FFIResult
acquire_uinput_keysink (Fd h) c = liftIO $ do
  cstr <- newCString $ unpack $ c^.keyboardName
  ffiReturn <$> c_acquire_uinput_keysink h cstr
    (fi $ c^.vendorCode) (fi $ c^.productCode) (fi $ c^.productVersion)

-- | Release a Uinput device
release_uinput_keysink :: MonadIO m => Fd -> m FFIResult
release_uinput_keysink (Fd h) = liftIO $ ffiReturn <$> c_release_uinput_keysink h

-- | Using a Uinput device, send a RawEvent to the Linux kernel
send_event ::
     Fd
  -> RawEvent
  -> RIO e ()
send_event (Fd h) e = liftIO $ void $ c_send_event h
  (fi $ e^.leType) (fi $ e^.leCode) (fi $ e^.leVal) (fi $ e^.leS) (fi $ e^.leNS)


--------------------------------------------------------------------------------

-- | Send a keycode to the OS
sendEvent :: KeySwitch -> RIO UinputEnv ()
sendEvent s = do
  h <- view kbf
  e <- case s^.switch of Press   -> mkRaw LinuxPress   $ s^.code
                         Release -> mkRaw LinuxRelease $ s^.code
  send_event h e

  -- Send the sync-event to signal to linux to update the driver state
  e <- mkSync
  send_event h e


-- | How to manage the context of having a Uinput keyboard
withUinput :: (LUIO m env)
  => UinputCfg -> Ctx r m PutKey
withUinput c = mkCtx $ \f -> do
  -- Run our maybe-command if specified
  traverse_ callCommand $ c^.preInit

  let init = do
        le <- view logEnv
        -- Open the file-handle to the standard uinput device
        fd <- liftIO $ openFd "/dev/uinput" WriteOnly Nothing $
                OpenFileFlags False False False True False

        -- Register the device-specs with the uinput kernel module
        logInfo "Registering uinput device"
        acquire_uinput_keysink fd c `onErr` \n
          -> throwing _UinputCouldNotCreate (c, n)

        -- Optionally, fork of a command to be run
        for_ (c^.postInit) $ \cmd -> do
          logInfo $ "Running post-uinput-init command: " <> (pack cmd)
          async . callCommand $ cmd

        UinputEnv c le fd <$> newMVar S.empty

  let cleanup env = do
        -- Unregister the device from the uinput kernel module
        logInfo "Unregistering uinput device"
        let h = env^.kbf
        let rel = release_uinput_keysink h `onErr` \n ->
              throwing _UinputCouldNotDestroy (c, n)
        let cls = liftIO $ closeFd h
        finally rel cls

  bracket init cleanup $ \env -> f (\e -> runRIO env $ sendEvent e)
