{-|
Module      : KMonad.Api.KeyIO.Linux.UinputSink
Description : Using Linux's uinput interface to emit events
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable
-}
module KMonad.Api.KeyIO.Linux.UinputSink
  ( UinputSink
  , UinputCfg(..)
  , keyboardName, vendorCode, productCode, productVersion, postInit
  , mkUinputSink
  , defUinputCfg
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Foreign.C.String
import Foreign.C.Types
import System.Process
import System.Posix

import KMonad.Core.Keyboard
import KMonad.Domain.Effect (nowIO)
import KMonad.Api.KeyIO.Types
import KMonad.Api.KeyIO.Linux.Types

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
makeClassy ''UinputSink

instance HasUinputCfg UinputSink where uinputCfg = cfg

-- | Return a new uinput 'KeySink' with extra options
mkUinputSink :: UinputCfg -> KeySink
mkUinputSink cfg' = BracketIO
  { _open  = usOpen cfg'
  , _close = usClose
  , _use   = usWrite
  }

-- | Create a new UinputSink
usOpen :: CanKeyIO e m => UinputCfg -> m UinputSink
usOpen cfg' = do
  let flgs = OpenFileFlags False False False True False
  fd  <- liftIO $ openFd "/dev/uinput" WriteOnly Nothing flgs
  ret <- liftIO $ acquire_uinput_keysink fd cfg'
  when (ret == -1) $ throwError (_SinkCreationError # "/dev/uinput")
  maybe (pure ()) (liftIO . void . forkIO . callCommand) (cfg'^.postInit)

  UinputSink cfg' <$> liftIO (newMVar fd)

-- | Close keysink
usClose :: UinputSink -> IO ()
usClose u = do
  fd  <- readMVar $ u^.st
  ret <- finally (release_uinput_keysink fd) (closeFd fd)
  when (ret == -1) $ throwError (_SinkDeletionError # "/dev/uinput")

-- | Write a keyboard event to the sink and sync the driver state. Using an MVar
-- ensures that we can never have 2 threads try to write at the same time.
usWrite :: UinputSink -> KeyEvent -> IO ()
usWrite u e = do
  fd <- takeMVar $ u^.st
  emit fd (e^.re _KeyEvent)
  emit fd =<< sync <$> nowIO
  putMVar (u^.st) fd
  where
    emit fd e' = do
      ret <- send_event fd e'
      when (ret == -1) $ throwError $ _SinkWriteError # ("/dev/uinput", e)
