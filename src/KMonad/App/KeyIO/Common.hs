module KMonad.App.KeyIO.Common

where

import KMonad.Prelude
import KMonad.Util.Keyboard

{- For KeyIO code shared across platforms. Put configuration-records here -}


--------------------------------------------------------------------------------
-- $types
--
-- Generally useful types used across all OSes

-- | Alias for an action that fetches a (Switch, Keycode) tuple from the OS.
type GetKey = OnlyIO (Switch, Keycode)

-- | Alias for an action that sends (Switch, Keycode) tuples to the OS.
type PutKey = (Switch, Keycode) -> OnlyIO ()

--------------------------------------------------------------------------------
-- $cfgs
--
-- The configuration records for all possible KeyIO specifications in KMonad.

-- | The configuration record for evdev key-input on Linux
data EvdevCfg = EvdevCfg
  { _evdevPath :: FilePath -- ^ The path to the input-file to open and capture
  } deriving Show
makeClassy ''EvdevCfg

--------------------------------------------------------------------------------
-- $util

  -- Snippet for inspiration: filter streams so only alternating press-release occurs
  --
  -- untilJust go where
  -- go = do
  --   decode <$> readChunk >>= \case
  --     Left  err -> throwIO err
  --     Right raw -> case select raw of
  --       Nothing -> pure Nothing
  --       Just (s, c) -> if s == Press
  --         then overMVar (view active) $ \cs -> pure $ if c `S.member` cs
  --           then (cs, Nothing) else (S.insert c cs, Just (s, c))
  --         else overMVar (view active) $ \cs -> pure $ if c `S.member` cs
  --           then (S.delete c cs, Just c) else (cs, Nothing)
