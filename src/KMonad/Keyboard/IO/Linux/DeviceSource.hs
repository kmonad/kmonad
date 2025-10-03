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

import GHC.IO.Exception (IOException(IOError, ioe_errno))

--------------------------------------------------------------------------------
-- $err

data DeviceSourceError
  = IOCtlGrabError    IOError
  | IOCtlReleaseError IOError
  | PathTypeMismatch  Bool FilePath
  | RootDirDoesNotExist FilePath
  | KeyIODecodeError  String
  deriving Exception

instance Show DeviceSourceError where
  show (IOCtlGrabError e)      = show e
  show (IOCtlReleaseError e)   = show e
  show (PathTypeMismatch d pt) = "Path exists but is not a " <> (if d then "directory" else "file") <> ": " <> pt
  show (RootDirDoesNotExist dev) = "Root directory for device '" <> dev <> "' does not exist"
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
    liftIO $ waitForPath lf False pt Nothing

  -- Test for existence followed by a inotify_add_watch
  waitForPath lf isDir pt' inot = do
    ptExists <- doesPathExist pt'
    unless ptExists $ case inot of
      Just inot' -> waitForPath' lf isDir pt' inot'
      Nothing -> withINotify $ \inot' -> do
        runRIO lf $ logInfo "Listening for device"
        waitForPath' lf isDir pt' inot'
        runRIO lf $ logInfo "Found device"
    let doesExistWithType = if isDir then doesDirectoryExist else doesFileExist
    foundWithType <- doesExistWithType pt'
    unless foundWithType . throwIO $ PathTypeMismatch isDir pt'

  -- Wait for parent path and add INotify watch
  waitForPath' lf isDir pt' inot = do
    let parent = takeDirectory pt'
    when (parent == pt') . throwIO $ RootDirDoesNotExist pt
    waitForPath lf True parent $ Just inot
    fn <- B.fromFilePath $ takeFileName pt'
    dir <- B.fromFilePath parent
    block <- newEmptyMVar
    runRIO lf $ logDebug $ "Waiting for path: " <> fromString pt'

    watch <- addWatch inot [Create, DeleteSelf] dir $ \case
      Created isDir' fn' | fn' == fn -> do
        runRIO lf . logDebug $ "Found path: " <> fromString pt'
        unless (isDir == isDir') . throwIO $ PathTypeMismatch isDir pt'
        putMVar block True
      DeletedSelf -> do
        runRIO lf . logDebug $ "Parent directory deleted: " <> fromString parent
        putMVar block False
      _ -> pure ()
    found <- takeMVar block
    removeWatch watch
    unless found $ waitForPath lf isDir pt' $ Just inot


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
    tryJust isENODEV (B.hGet hdl (src^.nbytes)) >>= \case
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
  isENODEV e@IOError{ioe_errno = Just errno}
    | src^.ignmis && Errno errno == eNODEV = Just e
  isENODEV _ = Nothing
