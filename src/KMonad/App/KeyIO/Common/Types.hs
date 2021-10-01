-- |
module KMonad.App.KeyIO.Common.Types where

import KMonad.Prelude
import KMonad.Util.Keyboard
import KMonad.Util.Name
import KMonad.Util.Time


{- For KeyIO code shared across platforms. Put configuration-records here -}



--------------------------------------------------------------------------------
-- $cfgs-linux
--
-- The configuration records for all of the Linux KeyIO options

-- | The configuration record for evdev key-input on Linux
newtype EvdevCfg = EvdevCfg
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

-- | The default uinput configuration
instance Default UinputCfg where
  def = UinputCfg
    { _vendorCode     = 0xFFFF
    , _productCode    = 0xFFFF
    , _productVersion = 0x0000
    , _keyboardName   = "KMonad simulated keyboard"
    , _preInit        = Nothing
    , _postInit       = Nothing
    }


--------------------------------------------------------------------------------
-- $cfgs-mac

-- | Holds the product string of keyboard if given. Else, Nothing.
newtype IOKitCfg = IOKitCfg
  { _productStr :: Maybe Text
  } deriving Show
makeClassy ''IOKitCfg

-- | By default, no product string (will grab all keyboards).
instance Default IOKitCfg where
  def = IOKitCfg Nothing

-- | No config options for Kext/Dext
data ExtCfg = ExtCfg deriving Show

--------------------------------------------------------------------------------
-- $cfgs-win

-- TODO: This is where the Win config records go

-- | No config options for the windows LowLevelHook
data LowLevelHookCfg = LowLevelHookCfg  deriving Show

-- | Placeholder
data SendEventCfg = SendEventCfg
  { _repDelay    :: Ms
  , _repInterval :: Ms
  }
  deriving Show
makeClassy ''SendEventCfg

instance Default SendEventCfg where
  def = SendEventCfg
    { _repDelay    = 300
    , _repInterval = 100
    }

--------------------------------------------------------------------------------
-- $cfgs-sum
--
-- TODO: Here is where the sum-type of input and output configs go

data KeyInputCfg
  = LinuxEvdevCfg          EvdevCfg
  | MacIOKitCfg            IOKitCfg
  | WindowsLowLevelHookCfg LowLevelHookCfg
  deriving Show


data KeyOutputCfg
  = LinuxUinputCfg      UinputCfg
  | MacExtCfg           ExtCfg
  | WindowsSendEventCfg SendEventCfg
  deriving Show


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
