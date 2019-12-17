{-|
Module      : KMonad.Api.KeyIO.Linux.DeviceSource
Description : Load and acquire a linux /dev/input device
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

-}
module KMonad.Api.KeyIO.Linux.DeviceSource
  ( deviceSource
  , deviceSource64

  , EventParser
  , decode64
  )
where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Data.Int
import Data.Word
import Foreign.C.Types
import GHC.IO.Handle (Handle)
import System.Posix
import UnliftIO

import qualified Data.Serialize as B (decode)
import qualified Data.ByteString as B

import KMonad.Core.Keyboard
import KMonad.Api.KeyIO.Types
import KMonad.Api.KeyIO.Linux.Types


--------------------------------------------------------------------------------

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

-- | Collection of data used to read from linux input.h event stream
data DeviceFile = DeviceFile
  { _pth    :: !FilePath     -- ^ Path to the event-file
  , _nbytes :: !Int          -- ^ Size of 1 input event in bytes
  , _prs    :: !EventParser  -- ^ Function to convert bytestring to event
  , _fd     :: !Fd           -- ^ Posix filedescriptor to the device file
  , _hdl    :: !Handle       -- ^ Haskell handle to the device file
  }
makeLenses ''DeviceFile


--------------------------------------------------------------------------------

-- | Open a device file
deviceSource
  :: Int         -- ^ The amount of bytes to read in 1 chunk
  -> EventParser -- ^ A function that parses LinuxEvent's from bytes
  -> FilePath    -- ^ The filepath to the device file
  -> KeySource
deviceSource n prs' pth' = BracketIO
  { _open  = lsOpen n prs' pth'
  , _close = lsClose
  , _use   = lsRead
  }

-- | Open a device file on a standard linux 64 bit architecture
deviceSource64
  :: FilePath  -- ^ The filepath to the device file
  -> KeySource
deviceSource64 = deviceSource 24 decode64


--------------------------------------------------------------------------------

-- | Open a handle to the keyboard, perform an ioctl grab and update internal state
lsOpen :: CanKeyIO e m => Int -> EventParser -> FilePath -> m DeviceFile
lsOpen n prs' pth' = do
  let flgs = OpenFileFlags False False False False False
  fd'  <- liftIO $ openFd pth' ReadOnly Nothing flgs
  hdl' <- liftIO $ fdToHandle fd'
  ret <- liftIO $ ioctl_keyboard fd' True
  when (ret == -1) $ throwError (_IOCtlGrabError # pth')
  return $ DeviceFile pth' n prs' fd' hdl'

-- | Release the ioctl grab and close the device file
lsClose :: CanKeyIO e m => DeviceFile -> m ()
lsClose src = do
  ret <- liftIO $ finally (ioctl_keyboard (src^.fd) False)
                          (closeFd (src^.fd))
  when (ret == -1) $ throwError (_IOCtlReleaseError # (src^.pth))

-- | Read a bytestring from an open filehandle and return a parsed event
lsRead :: CanKeyIO e m => DeviceFile -> m KeyEvent
lsRead src = do
  bts <- liftIO $ B.hGet (src^.hdl) (src^.nbytes)
  case (src^.prs $ bts) of
    Right e -> case e^?_KeyEvent of
      Just e' -> return e'
      Nothing -> lsRead src
    Left s -> throwError $ _EventParseError # (src^.pth, s)
