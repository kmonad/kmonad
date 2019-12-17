module KMonad.Domain.Effect.Effects.MonadEmit
  ( MonadEmit(..)
  , emitSeq
  , emitPress
  , emitRelease
  )
where

import KMonad.Core
import KMonad.Domain.Effect.Effects.MonadNow

-- | The 'MonadEmit' effect allows sending KeyEvent's to the OS.
class MonadNow m => MonadEmit m where
  emitKey :: KeyAction -> m ()

emitSeq :: (MonadEmit m) => KeySequence -> m ()
emitSeq = mapM_ emitKey

emitPress :: MonadEmit m => KeyCode -> m ()
emitPress = emitKey . mkKeyPress

emitRelease :: MonadEmit m => KeyCode -> m ()
emitRelease = emitKey . mkKeyRelease
