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

import KMonad.Prelude

import Foreign.C.Types
import System.Posix

import KMonad.Keyboard.IO.Linux.Types
import KMonad.Util

import qualified Data.Serialize as B (decode)
import qualified RIO.ByteString as B


--------------------------------------------------------------------------------
-- $ffi
foreign import ccall "ioctl_keyboard"
  c_ioctl_keyboard :: CInt -> CInt -> IO CInt

-- | Perform an IOCTL operation on an open keyboard handle
ioctl_keyboard
  :: Fd       -- ^ Descriptor to open keyboard file (like /dev/input/eventXX)
  -> Bool     -- ^ True to grab, False to ungrab
  -> IO Int   -- ^ Return the exit code
ioctl_keyboard (Fd h) b = fromIntegral <$>
  c_ioctl_keyboard h (if b then 1 else 0)


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
  { _pth    :: !FilePath     -- ^ Path to the event-file
  , _nbytes :: !Int          -- ^ Size of 1 input event in bytes
  , _prs    :: !EventParser  -- ^ Function to convert bytestring to event
  , _fd     :: !Fd           -- ^ Posix filedescriptor to the device file
  , _hdl    :: !Handle       -- ^ Haskell handle to the device file
  }
makeLenses ''DeviceFile

-- | Open a device file
deviceSource
  :: Int         -- ^ The amount of bytes to read in 1 chunk
  -> EventParser -- ^ A function that parses LinuxEvent's from bytes
  -> FilePath    -- ^ The filepath to the device file
  -> Acquire KeySource
deviceSource n prs pth
  = KeySource . lsRead <$> mkAcquire (lsOpen n prs pth) lsClose

-- | Open a device file on a standard linux 64 bit architecture
deviceSource64
  :: FilePath  -- ^ The filepath to the device file
  -> Acquire KeySource
deviceSource64 = deviceSource 24 decode64


--------------------------------------------------------------------------------
-- $io

-- | Open the keyboard, perform an ioctl grab and return a 'DeviceFile'
lsOpen :: Int -> EventParser -> FilePath -> IO DeviceFile
lsOpen n prs pth = do
  let flgs = OpenFileFlags False False False False False
  fd  <- openFd pth ReadOnly Nothing flgs
  hdl <- fdToHandle fd
  ioctl_keyboard fd True `onErr` IOCtlGrabError pth
  return $ DeviceFile pth n prs fd hdl

-- | Release the ioctl grab and close the device file
lsClose :: DeviceFile -> IO ()
lsClose src = finally (ioctl_keyboard (src^.fd) False) (closeFd (src^.fd))
  `onErr` IOCtlReleaseError (src ^. pth)

-- | Read a bytestring from an open filehandle and return a parsed event
lsRead :: DeviceFile -> IO KeyEvent
lsRead src = do
  bts <- B.hGet (src^.hdl) (src^.nbytes)
  case (src ^. prs $ bts) of
    Right p -> case fromLinuxKeyEvent p of
      Just e  -> return e
      Nothing -> lsRead src
    Left s -> throwIO $ EventParseError (src ^. pth) s
