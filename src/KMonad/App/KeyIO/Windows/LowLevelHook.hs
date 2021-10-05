{-|
Module      : KMonad.App.Key.IO.Windows.LowLevelHook
Description : Load and acquire a windows low-level keyboard hook.
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

-}
module KMonad.App.KeyIO.Windows.LowLevelHook
  ( withLowLevelHook )
where

import KMonad.Prelude

import Foreign.Marshal hiding (void)
import Foreign.Ptr
import Foreign.Storable

import KMonad.App.KeyIO.Common
import KMonad.Util
import KMonad.Util.Keyboard.Windows

--------------------------------------------------------------------------------

-- | Use the windows c-api to `grab` a keyboard
foreign import ccall "grab_kb"
  grab_kb :: OnlyIO Word8

-- | Release the keyboard hook
foreign import ccall "release_kb"
  release_kb :: OnlyIO Word8

-- | Pass a pointer to a buffer to wait_key, when it returns the buffer can be
-- read for the next key event.
foreign import ccall "wait_key"
  wait_key :: Ptr RawEvent -> OnlyIO ()


--------------------------------------------------------------------------------

-- | Data used to track `connection` to windows process
data LLHook = LLHook
  { _thread :: !(Async Word8)     -- ^ The thread-id of the listen-process
  , _buffer :: !(Ptr RawEvent) -- ^ Buffer used to communicate with process
  }
makeLenses ''LLHook

withLowLevelHook :: LUIO m e => LowLevelHookCfg -> Ctx r m GetKey
withLowLevelHook _ = mkCtx $ \f -> do
 
  let init = do
        logInfo "Registering low-level Windows keyboard hook"
        liftIO $ do
          tid <- async grab_kb
          buf <- mallocBytes $ sizeOf (undefined :: RawEvent)
          pure $ LLHook tid buf

  let cleanup env = do
        logInfo "Unregistering low-level Windows keyboard hook"
        liftIO $ do
          _ <- release_kb
          cancel $ env^.thread -- This might not be necessary, but it is safer
          free   $ env^.buffer

  let fetch env = do
        w <- liftIO $ (wait_key $ env^.buffer) >> (peek $ env^.buffer)
        pure $ mkKeySwitch (if w^.reVal == 0 then Press else Release)
                           (Keycode $ w^.reCode)

  bracket init cleanup $ \env -> f (fetch env)


-- -- | Return a KeySource using the Windows low-level hook approach.
-- llHook :: HasLogFunc e => RIO e (Acquire KeySource)
-- llHook = mkKeySource llOpen llClose llRead


--------------------------------------------------------------------------------

-- | Get a new 'KeyEvent' from Windows
-- --
-- -- NOTE: This can throw an error if the event fails to convert.
-- llRead :: HasLogFunc e => LLHook -> RIO e KeyEvent
-- llRead ll = do
--   we <- liftIO $ do
--     wait_key $ ll^.buffer
--     peek $ ll^.buffer
--   either throwIO pure $ fromWinKeyEvent we
