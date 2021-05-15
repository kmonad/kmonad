module KMonad.App.KeyIO.Linux.Evdev
  ( withEvdev )
where

import KMonad.Prelude
import KMonad.Object.Context
import KMonad.App.Logging

import Control.Exception.Lens

import Data.Text.Lens (_Text)

import Foreign.C.Types
import System.Posix

import KMonad.App.KeyIO.Linux.Common
import KMonad.App.KeyIO.Linux.Types

import qualified RIO.Set as S
import qualified RIO.ByteString as B
import qualified Data.Serialize as B (decode)


--------------------------------------------------------------------------------
-- $ffi

foreign import ccall "ioctl_keyboard"
  c_ioctl_keyboard :: CInt -> CInt -> OnlyIO CInt

-- | Perform an IOCTL operation on an open keyboard handle
ioctl_keyboard :: MonadIO m
  => Fd          -- ^ Descriptor to open keyboard file (like /dev/input/eventXX)
  -> Bool        -- ^ True to grab, False to ungrab
  -> m FFIResult -- ^ Return the exit code
ioctl_keyboard (Fd h) b = ffiReturn <$>
  liftIO (c_ioctl_keyboard h (if b then 1 else 0))


--------------------------------------------------------------------------------

data EvdevEnv = EvdevEnv
  { _le     :: LogEnv
  , _cfg    :: EvdevCfg
  , _hdl    :: Handle
  , _fd     :: Fd
  , _active :: MVar (S.Set Keycode)
  }
makeClassy ''EvdevEnv

instance HasEvdevCfg EvdevEnv where evdevCfg = cfg
instance HasLogEnv   EvdevEnv where logEnv   = le

--------------------------------------------------------------------------------

-- | Read a chunk of bytes from the device
--
-- FIXME: setup the KioResourceLost exception stuff
readChunk :: RIO EvdevEnv B.ByteString
readChunk = view hdl >>= flip B.hGet 24

-- | Parse an event from a series of bytes
decode :: B.ByteString -> Either KioException RawEvent
decode bs = let miss = CouldNotDecodeEvent . pack
                hit (a, b, c, d, e) = RawEvent e d c b a
            in bimap miss hit readBytes
  where
    readBytes :: Either String (Int32, Word16, Word16, Word64, Word64)
    readBytes = B.decode . B.reverse $ bs

-- | Extract only Press/Release events
select :: RawEvent -> Maybe (IsPress, Keycode)
select e
  | e^.leType == 1 && e^.leVal /= 2 = Just (e^.leVal == 1, Keycode $ e^.leCode)
  | otherwise                       = Nothing

-- | Read the next event from the evdev-file
--
-- NOTE: Here we filter out repeat events, or repeated push and release events.
-- We only ever emit the keycodes of alternating press/release events, starting
-- with a press event.
--
-- This can throw CouldNotDecodeEvent or KioResourceLost exceptions
--
nextEvent :: RIO EvdevEnv Keycode
nextEvent = untilJust go where
  go = do
    decode <$> readChunk >>= \case
      Left  err -> throwIO err
      Right raw -> case select raw of
        Nothing -> pure Nothing
        Just (isPress, c) -> if isPress
          then overMVar (view active) $ \cs -> pure $ if c `S.member` cs
            then (cs, Nothing) else (S.insert c cs, Just c)
          else overMVar (view active) $ \cs -> pure $ if c `S.member` cs
            then (S.delete c cs, Just c) else (cs, Nothing)

-- | Create a context of 'running with a captured input' for the provided
-- configuration.
--
-- This can throw any KioException
withEvdev :: (LUIO m env)
  => EvdevCfg -> Ctx r m GetKey
withEvdev c = mkCtx $ \f -> do

  let showErr t n = t <> " '" <> pack (c^.pth)
                      <> "'. With errorcode: " <> tshow n

  let init = do
        le <- view logEnv
        -- Open the posix evdev file and convert to handle
        let flgs = OpenFileFlags False False False False False
        fd' <- liftIO $ openFd (c^.pth) ReadOnly Nothing flgs
        hdl' <- liftIO $ fdToHandle fd'

        -- Execute ioctl-grab
        say_ LevelInfo $ "Initiating ioctl grab"
        ioctl_keyboard fd' True `onErr` \n
          -> throwing _CouldNotAcquireKio $ showErr "Failed to acquire" n

        -- Wrap up the environment
        EvdevEnv le c hdl' fd' <$> newMVar S.empty

  let cleanup env = do
        say_ LevelInfo $ "Releasing ioctl grab"
        ioctl_keyboard (env^.fd) True `onErr` \n
          -> throwing _CouldNotReleaseKio $ showErr "Failed to release" n
        liftIO . closeFd $ env^.fd

  bracket init cleanup $ \env -> f (runRIO env nextEvent)
