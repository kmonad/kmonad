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

  , EventParser
  , decode64
  )
where

import Prelude

import Foreign.C.Types
import System.Posix

import KMonad.Keyboard.IO.Linux.Types
import KMonad.Util
import KMonad.Runner

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
  show (IOCtlGrabError pth) = "Could not perform IOCTL grab on: " <> pth
  show (IOCtlReleaseError pth) = "Could not perform IOCTL release on: " <> pth
  show (KeyIODecodeError msg) = "Event decode failed with msg: " <> msg

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

-- | An EventParser parses a ByteString into a LinuxKeyEvent
type EventParser = B.ByteString -> Either String LinuxKeyEvent

-- | The EventParser that works on my 64-bit Linux environment
decode64 :: B.ByteString -> Either String LinuxKeyEvent
decode64 bs = (linuxKeyEvent . fliptup) <$> result
  where
    result :: Either String (Int32, Word16, Word16, Word64, Word64)
    result = B.decode . B.reverse $ bs

    fliptup (a, b, c, d, e) = (e, d, c, b, a)


--------------------------------------------------------------------------------
-- $types

-- | Collection of data used to read from linux input.h event stream
data DeviceFile = DeviceFile
  { _pth     :: !FilePath     -- ^ Path to the event-file
  , _nbytes  :: !Int          -- ^ Size of 1 input event in bytes
  , _prs     :: !EventParser  -- ^ Function to convert bytestring to event
  , _fd      :: !Fd           -- ^ Posix filedescriptor to the device file
  , _hdl     :: !Handle       -- ^ Haskell handle to the device file
  }
makeLenses ''DeviceFile

-- | Open a device file
deviceSource :: HasRunEnv e
  => Int         -- ^ The amount of bytes to read in 1 chunk
  -> EventParser -- ^ A function that parses LinuxEvent's from bytes
  -> FilePath    -- ^ The filepath to the device file
  -> RIO e (Acquire KeySource)
deviceSource n pr pt = mkKeySource (lsOpen n pr pt) lsClose lsRead

-- | Open a device file on a standard linux 64 bit architecture
deviceSource64 :: HasRunEnv e
  => FilePath  -- ^ The filepath to the device file
  -> RIO e (Acquire KeySource)
deviceSource64 = deviceSource 24 decode64


--------------------------------------------------------------------------------
-- $io

-- | Open the keyboard, perform an ioctl grab and return a 'DeviceFile'. This
-- can throw an 'IOException' if the file cannot be opened for reading, or an
-- 'IOCtlGrabError' if an ioctl grab could not be properly performed.
lsOpen :: (HasLogFunc e)
  => Int           -- ^ The size of 1 event in bytes
  -> EventParser   -- ^ The function that parser bytes to events
  -> FilePath      -- ^ The path to the device file
  -> RIO e DeviceFile
lsOpen n pr pt = do
  h  <- liftIO . openFd pt ReadOnly Nothing $
    OpenFileFlags False False False False False
  hd <- liftIO $ fdToHandle h
  logInfo $ "Initiating ioctl grab"
  ioctl_keyboard h True `onErr` IOCtlGrabError pt
  return $ DeviceFile pt n pr h hd

-- | Release the ioctl grab and close the device file. This can throw an
-- 'IOException' if the handle to the device cannot be properly closed, or an
-- 'IOCtlReleaseError' if the ioctl release could not be properly performed.
lsClose :: (HasLogFunc e) => DeviceFile -> RIO e ()
lsClose src = do
  logInfo $ "Releasing ioctl grab"
  ioctl_keyboard (src^.fd) False `onErr` IOCtlReleaseError (src^.pth)
  liftIO . closeFd $ src^.fd

-- | Read a bytestring from an open filehandle and return a parsed event. This
-- can throw a 'KeyIODecodeError' if reading from the 'DeviceSource' fails to
-- yield a parseable sequence of bytes.
lsRead :: (HasLogFunc e) => DeviceFile -> RIO e KeyEvent
lsRead src = do
  bts <- B.hGet (src^.hdl) (src^.nbytes)
  case (src^.prs $ bts) of
    Right p -> case fromLinuxKeyEvent p of
      Just e  -> return e
      Nothing -> lsRead src
    Left s -> throwIO $ KeyIODecodeError s
