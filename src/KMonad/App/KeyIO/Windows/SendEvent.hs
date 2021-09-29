{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-|
Module      : KMonad.App.KeyIO.Windows.SendEvent
Description : Using Windows' send_event functionality to inject KeyEvent's
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

This uses @sendKey@ from the @keyio_win.c@ to send keys to Windows. This itself
then uses the Windows 'SendInput' system call.

Since we filter out everything from KMonad's core except for alternating
press-release events, this breaks Window's key-repeat. We manually reintroduce
keyrepeat in this module.

-}
module KMonad.App.KeyIO.Windows.SendEvent
  ( withSendEvent
  )
where

import KMonad.Prelude

import Foreign.Ptr
import Foreign.Marshal hiding (void)
import Foreign.Storable

import KMonad.App.KeyIO.Common
import KMonad.Util
import KMonad.Util.Keyboard.Windows hiding (Keycode)

import qualified RIO.HashMap as M

--------------------------------------------------------------------------------

foreign import ccall "sendKey" c_sendKey :: Ptr RawEvent -> OnlyIO ()

--------------------------------------------------------------------------------

-- | A Hashmap of keys currently pressed
type ThreadMap = M.HashMap Keycode (Async ())

-- | The environment used to handle output operations
data SendEventEnv = SendEventEnv
  { _seCfg :: SendEventCfg   -- ^ The sink configuration
  , _prcs  :: MVar ThreadMap -- ^ Collection of currently running keys
  , _ptr   :: Ptr RawEvent   -- ^ Pointer used to communicate with Windows
  }
makeClassy ''SendEventEnv

instance HasSendEventCfg SendEventEnv where sendEventCfg = seCfg

type SIO a = RIO SendEventEnv a

-- | Send a key-event to Windows
sendKey :: EvType -> Keycode -> SIO ()
sendKey t c = do
  p <- view ptr
  liftIO $ do
    poke p (mkRaw t c)
    c_sendKey p

--------------------------------------------------------------------------------

-- | The stuff that happens after a key gets pressed
--
-- 1. Wait for the delay once, if reached, emit a keypress again
-- 2. Wait for the interval and, if reached, emit a keypress, forever
--
-- NOTE: we leave emitting the first press and the final release to the callers.
-- NOTE: this blocks, and so should only ever be forked
holdKey :: Keycode -> SIO ()
holdKey c = do
  let p = sendKey WindowsPress c
  delay <- view repDelay
  itvl  <- view repInterval
  wait delay >> p
  forever $ wait itvl >> p

-- | Helper function to run a function on the @prcs@ map
--
-- TODO: Maybe generalize this? I think this is a common pattern we use a lot
onPs :: (ThreadMap -> SIO ThreadMap) -> SIO ()
onPs f = view prcs >>= \pv -> modifyMVar pv (\ps -> (,()) <$> f ps)

-- | If the key is not pressed yet, press it and start the key-repeat thread
handlePress :: Keycode -> RIO SendEventEnv ()
handlePress c = onPs $ \ps -> if c `M.member` ps then pure ps else do
  sendKey WindowsPress c -- Send a press
  p <- async $ holdKey c -- Start the holding thread
  pure $ M.insert c p ps -- Store it in the mvar

-- | If the key is currently pressed, release it and stop the key-repeat thread
handleRelease :: Keycode -> SIO ()
handleRelease c = onPs $ \ps -> flip (maybe (pure ps)) (M.lookup c ps) $ \p -> do
  sendKey WindowsRelease c       -- Send a release
  cancel p                       -- Stop the holding thread
  pure $ M.delete c ps           -- Remove it from the mvar

-- | Handle any key event we are trying to send to the OS
handleEvent :: KeySwitch -> SIO ()
handleEvent e | isPress e = handlePress   $ e^.code
              | otherwise = handleRelease $ e^.code

--------------------------------------------------------------------------------

-- | The context of an active windows send-event output
withSendEvent :: LUIO m e => SendEventCfg -> Ctx r m PutKey
withSendEvent c = mkCtx $ \f -> do

  let init = do
        logInfo "Initializing Windows key sink"
        SendEventEnv c
          <$> newMVar M.empty
          <*> (liftIO $ mallocBytes (sizeOf (undefined :: RawEvent)))

  let cleanup e = do
        logInfo "Closing Windows key sink"
        pressed <- M.keys <$> (readMVar $ e^.prcs)
        runRIO e $ do
          mapM_ handleRelease pressed
          mapM_ (sendKey WindowsRelease) pressed
        liftIO . free $ e^.ptr

  bracket init cleanup $ \env -> f (\e -> runRIO env $ handleEvent e)

-- -- | Write an event to the pointer and prompt windows to inject it
-- --
-- -- NOTE: This can throw an error if event-conversion fails.
-- skSend :: (IO m, HasKeySwitch a) => Ptr RawEvent -> a -> m ()
-- skSend ptr a = do
--   let c = a^.keySwitch.code
--   let s = if a^.keySwitch.switch == Press then WindowsPress else WindowsRelease
--   liftIO $ do
--     poke ptr (mkRaw s c)
--     sendKey ptr
