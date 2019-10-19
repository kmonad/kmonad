module KMonad.Domain.Script

where

import Control.Lens
import Control.Monad.IO.Class

import System.IO
import System.Process

import KMonad.Core

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
  ( MonadTime m
  , MonadIO   m
  )

--------------------------------------------------------------------------------
-- $types

newtype ScriptMsg = ScriptMsg { unMsg :: Maybe KeyEvent }

terminate :: ScriptMsg
terminate = ScriptMsg Nothing

keyMsg :: KeyEvent -> ScriptMsg
keyMsg = ScriptMsg . Just


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
class Monad m => MonadScript m where
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
    mkOut x = case sfg^.scriptMode of
      RunOnly        -> pure Nothing
      StartAndSendIO -> pure $ out x
      StartAndCaptureIO -> holdAll >> pure (out x)

    out (Just stdin', Just stdout', _, _) = Just $ ScriptIO stdin' stdout'
    out _                                 = error "Calling out on RunOnly"

    mkIn = let raw = shell $ sfg^.scriptCmd in case sfg^.scriptMode of
      RunOnly -> raw
      _       -> raw { std_in = CreatePipe, std_out = CreatePipe }


--------------------------------------------------------------------------------
-- $comms

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

-- | Receive a 'ScriptMsg' from a managed script
recvScriptIO :: MonadIO m => ScriptIO -> m (Maybe ScriptMsg)
recvScriptIO io = liftIO $ do
  a <- hGet 1 $ io^.hOut
  if | a == 0 -> pure . Just $ terminate
     | a == 1 -> pure . Just $ keyMsg
  -- if a == 0
  --   then pure Nothing
  --   else do
  --     c <- hGet 1 $ io^.hOut
  --     pure . Just $ undefined

-- | Translate a series of bytes into Maybe a Maybe KeyEvent
-- fromBytes :: B.ByteString -> Maybe ScriptMsg
-- fromBytes bs
--   | B.null bs = Nothing
--   | B.length == 1 = undefined
