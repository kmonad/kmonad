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
import System.Posix

import KMonad.Keyboard.IO.Linux.Types
import KMonad.Util

import qualified Data.Serialize as B (decode)
import qualified RIO.ByteString as B

--------------------------------------------------------------------------------
-- $err

data DeviceSourceError
  = IOCtlGrabError    FilePath
  | IOCtlReleaseError FilePath
  | KeyIODecodeError  String
  deriving Exception

instance Show DeviceSourceError where
  show (IOCtlGrabError pth)    = "Could not perform IOCTL grab on: "    <> pth
  show (IOCtlReleaseError pth) = "Could not perform IOCTL release on: " <> pth
  show (KeyIODecodeError msg)  = "KeyEvent decode failed with msg: "    <> msg

makeClassyPrisms ''DeviceSourceError

--------------------------------------------------------------------------------
-- $ffi
foreign import ccall "ioctl_keyboard"
  c_ioctl_keyboard :: CInt -> CInt -> IO CInt

-- | Perform an IOCTL operation on an open keyboard handle
ioctl_keyboard :: MonadIO m
  => Fd      -- ^ Descriptor to open keyboard file (like /dev/input/eventXX)
  -> Bool    -- ^ True to grab, False to ungrab
  -> m Int   -- ^ Return the exit code
ioctl_keyboard (Fd h) b = fromIntegral <$>
  liftIO (c_ioctl_keyboard h (if b then 1 else 0))


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
  }
makeClassy ''DeviceSourceCfg

-- | Collection of data used to read from linux input.h event stream
data DeviceFile = DeviceFile
  { _cfg :: !DeviceSourceCfg -- ^ Configuration settings
  , _fd  :: !Fd              -- ^ Posix filedescriptor to the device file
  , _hdl :: !Handle          -- ^ Haskell handle to the device file
  }
makeClassy ''DeviceFile

instance HasDeviceSourceCfg DeviceFile where deviceSourceCfg = cfg
instance HasKeyEventParser  DeviceFile where keyEventParser  = cfg.parser

-- | Open a device file
deviceSource :: HasLogFunc e
  => KeyEventParser -- ^ The method by which to read and decode events
  -> FilePath    -- ^ The filepath to the device file
  -> RIO e (Acquire KeySource)
deviceSource pr pt = mkKeySource (lsOpen pr pt) lsClose lsRead

-- | Open a device file on a standard linux 64 bit architecture
deviceSource64 :: HasLogFunc e
  => FilePath  -- ^ The filepath to the device file
  -> RIO e (Acquire KeySource)
deviceSource64 = deviceSource defEventParser


--------------------------------------------------------------------------------
-- $io

-- | Open the keyboard, perform an ioctl grab and return a 'DeviceFile'. This
-- can throw an 'IOException' if the file cannot be opened for reading, or an
-- 'IOCtlGrabError' if an ioctl grab could not be properly performed.
lsOpen :: (HasLogFunc e)
  => KeyEventParser   -- ^ The method by which to decode events
  -> FilePath      -- ^ The path to the device file
  -> RIO e DeviceFile
lsOpen pr pt = do
#if MIN_VERSION_unix(2,8,0)
  h  <- liftIO . openFd pt ReadOnly $
#else
  h  <- liftIO . openFd pt ReadOnly Nothing $
#endif
    OpenFileFlags False False False False False
  hd <- liftIO $ fdToHandle h
  logInfo "Initiating ioctl grab"
  ioctl_keyboard h True `onErr` IOCtlGrabError pt
  return $ DeviceFile (DeviceSourceCfg pt pr) h hd

-- | Release the ioctl grab and close the device file. This can throw an
-- 'IOException' if the handle to the device cannot be properly closed, or an
-- 'IOCtlReleaseError' if the ioctl release could not be properly performed.
lsClose :: (HasLogFunc e) => DeviceFile -> RIO e ()
lsClose src = do
  logInfo "Releasing ioctl grab"
  ioctl_keyboard (src^.fd) False `onErr` IOCtlReleaseError (src^.pth)
  liftIO . closeFd $ src^.fd

-- | Read a bytestring from an open filehandle and return a parsed event. This
-- can throw a 'KeyIODecodeError' if reading from the 'DeviceFile' fails to
-- yield a parseable sequence of bytes.
lsRead :: (HasLogFunc e) => DeviceFile -> RIO e KeyEvent
lsRead src = do
  bts <- B.hGet (src^.hdl) (src^.nbytes)
  case src^.prs $ bts of
    Right p -> case fromLinuxKeyEvent p of
      Just e  -> return e
      Nothing -> lsRead src
    Left s -> throwIO $ KeyIODecodeError s
