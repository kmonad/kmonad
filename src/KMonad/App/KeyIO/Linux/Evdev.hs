module KMonad.App.KeyIO.Linux.Evdev
  ( withEvdev )
where

import KMonad.Prelude
import KMonad.Util.Ctx
import KMonad.Util.Logging
import KMonad.App.KeyIO.Common

import Control.Exception.Lens

import Data.Text.Lens (_Text)

import Foreign.C.Types
import System.Posix

import KMonad.Util.FFI
import KMonad.Util.Keyboard
import KMonad.Util.Keyboard.Linux

import qualified RIO.Set as S
import qualified RIO.ByteString as B
import qualified Data.Serialize as B (decode)

--------------------------------------------------------------------------------
-- $err

-- | All the things that can go wrong
data EvdevException
  = EvdevCouldNotAcquire EvdevCfg FailCode -- ^ Could not acquire IOCTL grab
  | EvdevCouldNotRelease EvdevCfg FailCode -- ^ Could not release IOCTL grab
  | EvdevResourceLost    EvdevCfg          -- ^ Resource disappeared
  | EvdevCouldNotDecode  EvdevCfg String   -- ^ Received unparseable input
  deriving Show
makeClassyPrisms ''EvdevException

-- | How to display EvdevExceptions
instance Exception EvdevException where
  displayException (EvdevCouldNotAcquire c n) = concat
    [ "Failed to acquire ioctl-grab on: '", show c
    , "' with errorcode: ", show n ]
  displayException (EvdevCouldNotRelease c n) = concat
    [ "Failed to release ioctl-grab on: '", show c
    , "' with errorcode: ", show n ]
  displayException (EvdevResourceLost c) = concat
    [ "Lost access to: '", show c, "' during execution."]
  displayException (EvdevCouldNotDecode c t) = concat
    [ "Failed to parse event from: '", show c
    , "' with error message: ", t ]

-- | Hooking up lensy exception handling
instance AsEvdevException SomeException where _EvdevException = exception


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
decode :: B.ByteString -> Either String RawEvent
decode bs = let hit (a, b, c, d, e) = RawEvent e d c b a
            in right hit readBytes
  where
    readBytes :: Either String (Int32, Word16, Word16, Word64, Word64)
    readBytes = B.decode . B.reverse $ bs

-- | Extract only Press/Release events
select :: RawEvent -> Maybe (Switch, Keycode)
select e
  | e^.leType == 1 && e^.leVal /= 2 = Just (if e^.leVal == 0 then Release else Press
                                           , Keycode $ e^.leCode)
  | otherwise                       = Nothing

-- | Read the next event from the evdev-file
--
-- Note that this filters out any event that is not a Press or Release. The
-- stream is not yet guaranteed to be alternating series of P->R->P etc..
--
nextEvent :: RIO EvdevEnv KeySwitch
nextEvent = untilJust $ decode <$> readChunk >>= \case
  Left err  -> throwing _EvdevCouldNotDecode . (, err) =<< view cfg
  Right raw -> case select raw of
    Nothing -> pure Nothing
    Just (s, c) -> pure $ Just $ mkKeySwitch s c

-- | Create a context of 'running with a captured input' for the provided
-- configuration.
--
-- This can throw any KioException
withEvdev :: (LUIO m env)
  => EvdevCfg -> Ctx r m GetKey
withEvdev c = mkCtx $ \f -> do

  let init = do
        le <- view logEnv
        -- Open the posix evdev file and convert to handle
        let flgs = OpenFileFlags False False False False False
        mFile <- getFile c  
        case mFile of
          Nothing -> throwing _EvdevCouldNotAcquire (c, 1)
          Just file -> do 
            fd'  <- liftIO $ openFd file ReadOnly Nothing flgs
            hdl' <- liftIO $ fdToHandle fd'

            -- Execute ioctl-grab
            logInfo "Initiating ioctl grab"
            ioctl_keyboard fd' True `onErr` \n
              -> throwing _EvdevCouldNotAcquire (c, n)
  
            -- Wrap up the environment
            pure $ EvdevEnv le c hdl' fd'

  let cleanup env = do
        logInfo "Releasing ioctl grab"
        ioctl_keyboard (env^.fd) True
          `onErr`   (\n -> throwing _EvdevCouldNotRelease (c, n)) -- throw proper error
          `finally` (liftIO . closeFd $ env^.fd)                  -- always close file

  bracket init cleanup $ \env -> f (runRIO env nextEvent)
