module KMonad.Domain.Effect.Effects.MonadScript

where

import Control.Lens
import Control.Monad (forever, void)
import Control.Monad.IO.Class

import System.IO
import System.Process
import UnliftIO (MonadUnliftIO(..))
import UnliftIO.Async

import KMonad.Core

import KMonad.Domain.Effect.Effects.MonadEmit
import KMonad.Domain.Effect.Effects.MonadFork
import KMonad.Domain.Effect.Effects.MonadNow


import qualified Data.ByteString as B


{-
Scripts can be spawned off from KMonad to allow for hooking arbitrary scripted
actions to the keyboard manager. The following options should exist:

- launch a script and forget about it

- launch a script and open input-output communication with it using stdin and
  stdout

- launch a script and open input-output communication with it using stdin and
  stdout *and also* stop further processing of input events until the script
  terminates.
-}

type CanScript m =
  ( MonadNow m
  , MonadIO   m
  )

--------------------------------------------------------------------------------
-- $types

newtype ScriptMsg = ScriptMsg { unMsg :: Maybe KeyAction }

terminate :: ScriptMsg
terminate = ScriptMsg Nothing

keyMsg :: KeyAction -> ScriptMsg
keyMsg = ScriptMsg . Just

-- keyMsgNow :: MonadNow m => SwitchState -> KeyCode -> m ScriptMsg
-- keyMsgNow x c = withNow $ \t -> (ScriptMsg . Just $ mkKeyEvent x c t)

data ScriptMode
  = RunOnly           -- ^ Start a script and forget about it
  | StartAndSendIO    -- ^ Start a script and do stdin-stdout communication
  | StartAndCaptureIO -- ^ Start a script and do io and stop kmonad normal processing
  deriving (Eq, Ord, Show)


data ScriptCfg = ScriptCfg
  { _scriptMode :: ScriptMode
  , _scriptCmd  :: String
  }

data ScriptIO = ScriptIO
  { _hIn  :: Handle
  , _hOut :: Handle
  }

makeLenses ''ScriptCfg
makeLenses ''ScriptIO

--------------------------------------------------------------------------------
-- $script

-- | This effect allows for launching external scripts
class (MonadNow m, MonadEmit m) => MonadScript m where
  startScript :: ScriptCfg -> m (Maybe ScriptIO)
  sendScript  :: ScriptIO -> ScriptMsg -> m ()
  recvScript  :: ScriptIO -> m ScriptMsg

--------------------------------------------------------------------------------
-- $io_impl

-- | This needs to be replaced by an implementation where managers can request
-- copies or captures of the full input stream.
holdAll :: MonadIO m => m ()
holdAll = undefined

startScriptIO :: MonadIO m => ScriptCfg -> m (Maybe ScriptIO)
startScriptIO sfg = mkOut =<< (liftIO . createProcess $ mkIn)
  where
    -- Dispatch on script-type to decide launching method
    mkOut x = case sfg^.scriptMode of
      RunOnly           -> pure Nothing
      StartAndSendIO    -> pure $ out x
      StartAndCaptureIO -> holdAll >> pure (out x)

    -- translate createProcess output to ScriptIO
    out (Just stdin', Just stdout', _, _) = Just $ ScriptIO stdin' stdout'
    out _                                 = error "Calling out on RunOnly" -- should not be reachable

    -- construct a valid CreateProcess record from a ScriptCfg
    mkIn = let raw = shell $ sfg^.scriptCmd in case sfg^.scriptMode of
      RunOnly -> raw
      _       -> raw { std_in = CreatePipe, std_out = CreatePipe }

manageScript :: (MonadScript m, MonadEmit m, MonadUnliftIO m)
  => m KeyEvent -- ^ An action that retrieves the next KeyEvent from KMonad
  -> ScriptIO   -- ^ The IO interface to the script
  -> m ()       -- ^ An action that will manage communication between KMonad and
                --   the script untill either terminates. (blocking)
manageScript nxt sio = do
  void $ race reader writer
  where
    reader = forever $ nxt >>= sendScript sio . keyMsg . (view keyAction)
    writer = recvScript sio >>= \case
      ScriptMsg Nothing  -> pure ()
      ScriptMsg (Just e) -> emitKey e >> writer


forkScript :: (MonadScript m, MonadEmit m, MonadUnliftIO m, MonadFork m)
  => m KeyEvent -- ^ An action that retrieves the next KeyEvent from KMonad
  -> ScriptIO   -- ^ The IO interface to the script
  -> m ()       -- ^ An action that will manage communication between KMonad and
                --   the script untill either terminates. (non-blocking)
forkScript nxt = fork . manageScript nxt

-- forkScript :: (MonadIO m, MonadScript m)
-- launchManager :: (MonadIO m, MonadScript m)
--   => Bool        -- ^ Whether to capture or not
--   -> m KeyAction -- ^
--   -> ScriptIO    -- ^ The IO interface to the script
--   -> m ()        -- ^ The resulting action
-- launchManager cap sio = do
--   let f = do

--   if cap then liftIO . forkIO $ f else f

--   where reader =


-- manageScriptIO :: (MonadIO m, MonadScript m) => ScriptIO -> m ()
-- manageScriptIO sio = recvScript sio >>= \case
--   ScriptMsg Nothing  -> pure ()
--   ScriptMsg (Just e) -> emitKey e >>

--------------------------------------------------------------------------------
-- $comms
--
-- We support the ability to communicate with a script through the stdin and
-- stdout streams. If a script is launched with 'StartAndSendIO' or
-- 'StartAndCaptureIO', then KMonad will send all key-events through the
-- script's stdin through the following 2 byte encoding:
-- 1st byte:
--   0 - signal to terminate
--   1 - Received a key press
--   2 - Received a key release
-- 2nd byte:
--   If the first byte was 1 or 2, the second byte contains the integer
--   equivalent of the KeyCode Enum (which lines up with Linux's event-codes).
--
-- The same messaging interface can be used to send messages from the script to
-- KMonad, except now 0 signals KMonad to terminate the script, and key-events
-- will be emitted using whatever KeySink KMonad is currently configured to use.
--

-- | Translate a Maybe KeyEvent to a ByteString for process communication
-- Nothing -> [0], should signal termination
-- Just e  -> [<press=1, release=2>, linux-keycode]
toBytes :: ScriptMsg -> B.ByteString
toBytes (ScriptMsg Nothing)  = B.singleton 0
toBytes (ScriptMsg (Just e)) = B.pack
  [ if e^.switchState == Engaged then 1 else 2
  , fromIntegral . fromEnum $ e^.keyCode ]

-- | Send a 'ScriptMsg' to a managed script
sendScriptIO :: MonadIO m => ScriptIO -> ScriptMsg -> m ()
sendScriptIO io msg = liftIO $ B.hPut (io^.hIn) (toBytes msg)

-- | Incrementaly receive a 'ScriptMsg' from a managed script
recvScriptIO :: MonadIO m => ScriptIO -> m (Maybe ScriptMsg)
recvScriptIO io = let getOne = B.head <$> B.hGet (io^.hOut) 1 in liftIO $ do
  a <- getOne
  c <- toEnum . fromIntegral <$> getOne -- This should only read if evaluated.. right?
  pure $ if | a == 0 -> Just terminate
            | a == 1 -> Just . keyMsg $ KeyAction Engaged    c
            | a == 2 -> Just . keyMsg $ KeyAction Disengaged c
            | True   -> Nothing
