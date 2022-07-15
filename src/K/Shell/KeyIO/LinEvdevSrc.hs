-- |
{-# LANGUAGE CPP #-}

module K.Shell.KeyIO.LinEvdevSrc where

import K.Shell.KeyIO.Initial

import Foreign.C.Types
import System.IO (hSetBinaryMode)

import qualified Control.Exception.Lens as Exc
import qualified Data.Binary            as B
import qualified Data.Binary.Get        as B
import qualified RIO.ByteString.Lazy    as L --(hGetContents)

#if defined linux_HOST_OS
import System.Posix
#endif


-- error -----------------------------------------------------------------------

data IoctlError
  = IoctlGrabError
  | IoctlReleaseError
  deriving Eq
makeClassyPrisms ''IoctlError

instance Show IoctlError where
  show IoctlGrabError
    = "Could not perform IOCTL grab"
  show IoctlReleaseError
    = "Could not perform IOCTL release"

instance Exception IoctlError
instance AsIoctlError SomeException where _IoctlError = _SomeException

-- ffi -------------------------------------------------------------------------

#if defined linux_HOST_OS

foreign import ccall "ioctl_keyboard"
  c_ioctl_keyboard :: CInt -> CInt -> IO CInt

#else

c_ioctl_keyboard :: CInt -> CInt -> IO CInt
c_ioctl_keyboard _ _ = excThrowing _OSError (Linux, msg)
  where msg = "LinEvdevSrc only works on Linux"

#endif

-- | Try to acquire an IOCTL grab on a file descriptor
acquireIoctl :: MonadIO m => Fd -> m ()
acquireIoctl (Fd h) = liftIO (c_ioctl_keyboard h 1)
  `ffiErr` excThrowing _IoctlGrabError ()

-- | Try to release an IOCTL grab on a file descriptor
releaseIoctl :: MonadIO m => Fd -> m ()
releaseIoctl (Fd h) = liftIO (c_ioctl_keyboard h 0)
  `ffiErr`  excThrowing _IoctlReleaseError ()

-- basic types -----------------------------------------------------------------

data EvdevEnv = EvdevEnv
  { _fd  :: !Fd         -- ^ Posix filedescriptor to device file
  , _hdl :: !Handle     -- ^ Haskell handle to device file
  , _pth :: !FilePath   -- ^ Path to the device file
  -- , _buf :: LByteString -- ^ Lazy 'Bytestring' of keyboard input
  , _le  :: !LogEnv     -- ^ Reference to the LogEnv
  }
makeClassy ''EvdevEnv

-- ops -------------------------------------------------------------------------

withLinEvdevSrc :: (UIO m, CanLog env m) => Path -> (KeySrc -> m a) -> m a
withLinEvdevSrc p f = bracket (getEvdev p) relEvdev $ \devenv -> do
  f $ KeySrc (runRIO devenv readEvdev)

getEvdev :: (UIO m, CanLog env m) => Path -> m EvdevEnv
getEvdev p = do
  pth <- resolve p
  le  <- view logEnv
  fd  <- liftIO . openFd pth ReadOnly Nothing $
           OpenFileFlags False False False False False
  hd  <- liftIO $ fdToHandle fd
  liftIO $ hSetBinaryMode hd True
  -- bf <- L.hGetContents hd

  logInfo $ "Acquiring evdev device: " <> pack pth
  acquireIoctl fd `onException` liftIO (closeFd fd)
  pure $ EvdevEnv fd hd pth {-bf-} le

relEvdev :: (UIO m, CanLog env m) => EvdevEnv -> m ()
relEvdev e = do
  logInfo $ "Releasing evdev device: " <> pack (e^.pth)
  releaseIoctl (e^.fd) `finally` liftIO (closeFd $ e^.fd)

readEvdev :: RIO EvdevEnv KioEvent
readEvdev = do
  -- b <- view buf
  -- forever $ do
  h <- view hdl
  x <- L.hGet h 24
    -- let b = B.runGet getLinEvent x
    -- let e = b ^? _LinEvent
    -- traceShowIO (b, e)
  maybe readEvdev pure $ B.runGet getLinEvent x ^? _LinEvent

  --   Nothing -> readEvdev
  --   Just e  -> pure e
  -- -- forever $ do
  --   x <- L.hGet h 24
  --   let e = B.runGet getLinEvent x
  --   traceShowIO e
  -- -- traceIO "ping"
  -- --  traceShowIO buf
  -- -- traceIO "pong"

  -- pure undefined

  -- -- forever $ do

  --   b <- L.take 1 <$> view buf
  --   traceShowIO b

  -- traceShowIO =<< view buf
  -- undefined
  -- traceIO $ "a"
  -- e <- B.decode <$> view buf
  -- traceIO $ "b"
  -- c <- maybe readEvdev pure $ e^?_LinEvent
  -- traceIO $ "c"
  -- pure c
