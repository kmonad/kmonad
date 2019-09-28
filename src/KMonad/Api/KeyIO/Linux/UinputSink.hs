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
  , uinputSink
  )
where

import Control.Concurrent.MVar
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Foreign.C.Types
import System.Posix

import KMonad.Core.Keyboard
import KMonad.Domain.Effect (nowIO)
import KMonad.Api.KeyIO.Types
import KMonad.Api.KeyIO.Linux.Types



--------------------------------------------------------------------------------
-- FFI calls and type-friendly wrappers

foreign import ccall "acquire_uinput_keysink"
  c_acquire_uinput_keysink :: CInt -> IO Int

foreign import ccall "release_uinput_keysink"
  c_release_uinput_keysink :: CInt -> IO Int

foreign import ccall "send_event"
  c_send_event :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO Int

-- | Create and acquire a Uinput device
acquire_uinput_keysink :: Fd -> IO Int
acquire_uinput_keysink (Fd h) = c_acquire_uinput_keysink h

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
newtype UinputSink = UinputSink { st :: MVar Fd }

-- | Return a new uinput 'KeySink'
uinputSink :: KeySink
uinputSink = KeySink
  { snkOpen  = usOpen
  , snkClose = usClose
  , snkWrite = usWrite
  }

-- | Create a new UinputSink
usOpen :: CanKeyIO e m => m UinputSink
usOpen = do
  let flgs = OpenFileFlags False False False True False
  fd  <- liftIO $ openFd "/dev/uinput" WriteOnly Nothing flgs
  ret <- liftIO $ acquire_uinput_keysink fd
  when (ret == -1) $ throwError (_SinkCreationError # "/dev/uinput")
  UinputSink <$> liftIO (newMVar fd)

-- | Close keysink
usClose :: UinputSink -> IO ()
usClose u = do
  fd  <- readMVar . st $ u
  ret <- finally (release_uinput_keysink fd) (closeFd fd)
  when (ret == -1) $ throwError (_SinkDeletionError # "/dev/uinput")

-- | Write a keyboard event to the sink and sync the driver state. Using an MVar
-- ensures that we can never have 2 threads try to write at the same time.
usWrite :: UinputSink -> KeyEvent -> IO ()
usWrite u e = do
  fd <- takeMVar . st $ u
  emit fd (e^.re _KeyEvent)
  emit fd =<< sync <$> nowIO
  putMVar (st u) fd
  where
    emit fd e' = do
      ret <- send_event fd e'
      when (ret == -1) $ throwError $ _SinkWriteError # ("/dev/uinput", e)
