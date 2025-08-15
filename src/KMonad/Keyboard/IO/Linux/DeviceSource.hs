{-# LANGUAGE CPP            #-}
{-# LANGUAGE DeriveAnyClass #-}
{-|
Module      : KMonad.Keyboard.IO.Linux.DeviceSource
Description : Load and acquire a linux /dev/input device
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

-}
module KMonad.Keyboard.IO.Linux.DeviceSource
  ( deviceSource
  , deviceSource64

  , KeyEventParser
  , decode64
  )
where

import KMonad.Prelude
import Foreign.C.Types
import Foreign.C.Error
import System.Posix
import System.IO.Error

import KMonad.Keyboard.IO.Linux.Types
import KMonad.Util

import qualified Data.Serialize as B (decode)
import qualified RIO.ByteString as B

import System.INotify
import RIO.Directory
import RIO.FilePath

--------------------------------------------------------------------------------
-- $err

data DeviceSourceError
  = IOCtlGrabError    IOError
  | IOCtlReleaseError IOError
  | KeyIODecodeError  String
  deriving Exception

instance Show DeviceSourceError where
  show (IOCtlGrabError e)      = show e
  show (IOCtlReleaseError e)   = show e
  show (KeyIODecodeError msg)  = "KeyEvent decode failed with msg: "    <> msg

makeClassyPrisms ''DeviceSourceError

--------------------------------------------------------------------------------
-- $ffi
foreign import ccall "ioctl_keyboard"
  c_ioctl_keyboard :: Fd -> CInt -> IO CInt

-- | Perform an IOCTL operation on an open keyboard handle
ioctl_keyboard :: MonadIO m
  => Fd        -- ^ Descriptor to open keyboard file (like /dev/input/eventXX)
  -> FilePath  -- ^ FilePath to keyboard for error reporting
  -> Bool      -- ^ True to grab, False to ungrab
  -> m ()      -- ^ Return the exit code
ioctl_keyboard h pt g = liftIO $ do
  throwErrnoPathIfMinus1_
    ("Could not perform IOCTL " ++ if g then "grab" else "release")
    pt
    (c_ioctl_keyboard h $ if g then 1 else 0)


--------------------------------------------------------------------------------
-- $decoding

-- | A 'KeyEventParser' describes how to read and parse 'LinuxKeyEvent's from
-- the binary data-stream provided by the device-file.
data KeyEventParser = KeyEventParser
  { _nbytes :: !Int
    -- ^ Size of 1 input event in bytes
  , _prs    :: !(B.ByteString -> Either String LinuxKeyEvent)
    -- ^ Function to convert bytestring to event
  }
makeClassy ''KeyEventParser

-- | Default configuration for parsing keyboard events
defEventParser :: KeyEventParser
defEventParser = KeyEventParser 24 decode64

-- | The KeyEventParser that works on my 64-bit Linux environment
decode64 :: B.ByteString -> Either String LinuxKeyEvent
decode64 bs = linuxKeyEvent . fliptup <$> result
  where
    result :: Either String (Int32, Word16, Word16, Word64, Word64)
    result = B.decode . B.reverse $ bs

    fliptup (a, b, c, d, e) = (e, d, c, b, a)


--------------------------------------------------------------------------------
-- $types

-- | Configurable components of a DeviceSource
data DeviceSourceCfg = DeviceSourceCfg
  { _pth     :: !FilePath        -- ^ Path to the event-file
  , _parser  :: !KeyEventParser  -- ^ The method used to decode events
  , _ignmis  :: !Bool            -- ^ Whether to wait for keyboard to (re-)appear
  }
makeClassy ''DeviceSourceCfg

-- | Collection of data used to read from linux input.h event stream
data DeviceFile = DeviceFile
  { _cfg :: !DeviceSourceCfg -- ^ Configuration settings
  , _dev :: !(IORef (Fd, Handle)) -- ^ Posix and Haskell filedescriptor to the device file
  }
makeClassy ''DeviceFile

instance HasDeviceSourceCfg DeviceFile where deviceSourceCfg = cfg
instance HasKeyEventParser  DeviceFile where keyEventParser  = cfg.parser

-- | Open a device file
deviceSource :: HasLogFunc e
  => KeyEventParser -- ^ The method by which to read and decode events
  -> FilePath    -- ^ The filepath to the device file
  -> Bool           -- ^ Whether to wait for keyboard to (re-)appear
  -> RIO e (Acquire KeySource)
deviceSource pr pt im = mkKeySource (lsOpen pr pt im) lsClose lsRead

-- | Open a device file on a standard linux 64 bit architecture
deviceSource64 :: HasLogFunc e
  => FilePath  -- ^ The filepath to the device file
  -> Bool           -- ^ Whether to wait for keyboard to (re-)appear
  -> RIO e (Acquire KeySource)
deviceSource64 = deviceSource defEventParser


--------------------------------------------------------------------------------
-- $io

-- | Open the keyboard, perform an ioctl grab and return the device handles. This
-- can throw an 'IOException' if the file cannot be opened for reading, or an
-- 'IOCtlGrabError' if an ioctl grab could not be properly performed.
lsOpen' :: HasLogFunc e => FilePath -> Bool -> RIO e (Fd, Handle)
lsOpen' pt im = do
  when im waitForDeviceToExists

  fd <- liftIO $ openFd pt
    ReadOnly
#if !MIN_VERSION_unix(2,8,0)
    Nothing
#endif
    defaultFileFlags
  hd <- liftIO $ fdToHandle fd
  logInfo "Initiating ioctl grab"
  ioctl_keyboard fd pt True `catch` (throwIO . IOCtlGrabError)
  return (fd, hd)
 where
  waitForDeviceToExists = do
    lf <- view logFuncL
    devExists <- doesFileExist pt
    unless devExists . liftIO . withINotify $ \inot -> do
      runRIO lf $ logInfo "Listening for device"
      rpt <- B.fromFilePath $ takeFileName  pt
      dir <- B.fromFilePath $ takeDirectory pt
      block <- newEmptyMVar
      _ <- addWatch inot [Create] dir $ \case
        Created _ rpt' | rpt == rpt' -> putMVar block ()
        _ -> pure ()
      takeMVar block

-- | Like `lsOpen'` but wrap it in a full 'DeviceFile'.
lsOpen :: (HasLogFunc e)
  => KeyEventParser   -- ^ The method by which to decode events
  -> FilePath      -- ^ The path to the device file
  -> Bool             -- ^ Whether to wait for keyboard to (re-)appear
  -> RIO e DeviceFile
lsOpen pr pt im = DeviceFile (DeviceSourceCfg pt pr im) <$> (newIORef =<< lsOpen' pt im)

-- | Release the ioctl grab and close the device file. This can throw an
-- 'IOException' if the handle to the device cannot be properly closed, or an
-- 'IOCtlReleaseError' if the ioctl release could not be properly performed.
lsClose :: (HasLogFunc e) => DeviceFile -> RIO e ()
lsClose src = do
  (fd, _) <- readIORef (src^.dev)
  logInfo "Releasing ioctl grab"
  ioctl_keyboard fd (src^.cfg.pth) False `catch` (throwIO . IOCtlReleaseError)
  liftIO $ closeFd fd

-- | Read a bytestring from an open filehandle and return a parsed event. This
-- can throw a 'KeyIODecodeError' if reading from the 'DeviceFile' fails to
-- yield a parseable sequence of bytes.
lsRead :: (HasLogFunc e) => DeviceFile -> RIO e KeyEvent
lsRead src = do
  bts <- lsRead' =<< readIORef (src^.dev)
  case src^.prs $ bts of
    Right p -> case fromLinuxKeyEvent p of
      Just e  -> return e
      Nothing -> lsRead src
    Left s -> throwIO $ KeyIODecodeError s
 where
  lsRead' (fd, hdl) =
    tryJust retriable (B.hGet hdl (src^.nbytes)) >>= \case
      Right bts -> pure bts
      Left e -> do
        devExists <- doesFileExist (src^.cfg.pth)
        liftIO $ closeFd fd
        when devExists $ logRethrow "Device still exists, but reading failed" (toException e)
        logInfo "Device disconnected"
        h <- lsOpen' (src^.cfg.pth) (src^.ignmis)
        writeIORef (src^.dev) h
        logInfo "Device reconnected"
        lsRead' h
  retriable e = if src^.ignmis && isIllegalOperation e
    then Just e
    else Nothing
