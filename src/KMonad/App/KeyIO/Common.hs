module KMonad.App.KeyIO.Common

where

import KMonad.Prelude
import KMonad.Util.Keyboard
import KMonad.Util.Name


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
-- $cfgs-linux
--
-- The configuration records for all of the Linux KeyIO options

-- | The configuration record for evdev key-input on Linux
data EvdevCfg = EvdevCfg
  { _evdevPath :: FilePath -- ^ The path to the input-file to open and capture
  } deriving Show
makeClassy ''EvdevCfg

-- | Configuration of the Uinput keyboard to instantiate
data UinputCfg = UinputCfg
  { _vendorCode     :: !Int  -- ^ USB vendor code of the generated keyboard
  , _productCode    :: !Int  -- ^ USB product code of the generated keyboard
  , _productVersion :: !Int  -- ^ USB product version
  , _keyboardName   :: !Name -- ^ Name used to identify keyboard to OS
  , _preInit        :: !(Maybe String)
    -- ^ Optionally, a command to run before trying to open a uinput keyboard
  , _postInit       :: !(Maybe String)
    -- ^ Optionally, a command to execute after keyboard has been generated
  } deriving (Eq, Show)
makeClassy ''UinputCfg

--------------------------------------------------------------------------------
-- $cfgs-mac

-- TODO: This is where the Mac config records go

--------------------------------------------------------------------------------
-- $cfgs-win

-- TODO: This is where the Win config records go



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
