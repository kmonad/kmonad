{-|
Module      : KMonad.Keyboard.IO.Linux.UinputSink
Description : Using Linux's uinput interface to emit events
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable
-}
module KMonad.Keyboard.IO.Linux.UinputSink
  ( UinputSink
  , UinputCfg(..)
  , keyboardName
  , vendorCode
  , productCode
  , productVersion
  , postInit
  , uinputSink
  , defUinputCfg
  )
where

import KMonad.Prelude

import Foreign.C.String
import Foreign.C.Types
import System.Posix
import UnliftIO.Async   (async)
import UnliftIO.Process (callCommand)

import KMonad.Keyboard.IO.Linux.Types
import KMonad.Util


--------------------------------------------------------------------------------
-- $cfg

-- | Configuration of the Uinput keyboard to instantiate
data UinputCfg = UinputCfg
  { _vendorCode     :: CInt
  , _productCode    :: CInt
  , _productVersion :: CInt
  , _keyboardName   :: String
  , _postInit       :: Maybe String
  } deriving (Eq, Show)
makeClassy ''UinputCfg

-- | Default Uinput configuration
defUinputCfg :: UinputCfg
defUinputCfg = UinputCfg
  { _vendorCode     = 0x1235
  , _productCode    = 0x5679
  , _productVersion = 0x0000
  , _keyboardName   = "KMonad simulated keyboard"
  , _postInit       = Nothing
  }

--------------------------------------------------------------------------------
-- FFI calls and type-friendly wrappers

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

-- | Create and acquire a Uinput device
acquire_uinput_keysink :: Fd -> UinputCfg -> IO Int
acquire_uinput_keysink (Fd h) cfg = do
  cstr <- newCString $ cfg^.keyboardName
  c_acquire_uinput_keysink h cstr (cfg^.vendorCode) (cfg^.productCode) (cfg^.productVersion)

-- | Release a Uinput device
release_uinput_keysink :: Fd -> IO Int
release_uinput_keysink (Fd h) = c_release_uinput_keysink h

-- | Using a Uinput device, send a KeyEvent to the Linux kernel
send_event :: Fd -> LinuxKeyEvent -> IO Int
send_event (Fd h) (LinuxKeyEvent (s', ns', typ, c, val))
  = c_send_event h typ c val s' ns'


--------------------------------------------------------------------------------
-- UinputSink definition and implementation

-- | UinputSink is an MVar to a filehandle
data UinputSink = UinputSink
  { _cfg :: UinputCfg
  , _st  :: MVar Fd
  }
makeLenses ''UinputSink

-- | Return a new uinput 'KeySink' with extra options
uinputSink :: UinputCfg -> Acquire KeySink
uinputSink cfg = KeySink . usWrite <$> mkAcquire (usOpen cfg) usClose

-- | Create a new UinputSink
usOpen :: UinputCfg -> IO UinputSink
usOpen cfg = do
  let flgs = OpenFileFlags False False False True False
  fd  <- openFd "/dev/uinput" WriteOnly Nothing flgs
  acquire_uinput_keysink fd cfg `onErr` SinkCreationError (cfg ^. keyboardName)
  maybe (pure ()) (void . async . callCommand) (cfg^.postInit)
  UinputSink cfg <$> newMVar fd

-- | Close keysink
usClose :: UinputSink -> IO ()
usClose u = withMVar (u^.st) $ \fd -> do
  finally (release_uinput_keysink fd) (closeFd fd)
    `onErr` SinkDeletionError (u ^. cfg . keyboardName)

-- | Write a keyboard event to the sink and sync the driver state. Using an MVar
-- ensures that we can never have 2 threads try to write at the same time.
usWrite :: UinputSink -> KeyAction -> IO ()
usWrite u a = withMVar (u^.st) $ \fd -> do
  -- Translate the key action to a LinuxKeyEvent
  e <- toLinuxKeyEvent <$> (now $ flip atTime a)
  send_event fd e `onErr` SinkWriteError (u ^. cfg . keyboardName)
  (now sync >>= send_event fd) `onErr` SinkWriteError (u ^. cfg . keyboardName)
