module KMonad.App.KeyIO.Common where

import qualified RIO.Text as T 
import System.Directory (listDirectory)
import qualified Data.List as L 
import KMonad.Prelude
import KMonad.Util.Keyboard
import KMonad.Util.Name


{- For KeyIO code shared across platforms. Put configuration-records here -}



--------------------------------------------------------------------------------
-- $cfgs-linux
--
-- The configuration records for all of the Linux KeyIO options

-- | The configuration record for evdev key-input on Linux
data EvdevCfg =
  -- | The filepath is known beforehand
  EvdevCfg !FilePath
  -- | Search for a `Fix` within a set of paths (directories)
  -- Ideally, we'd have liked to use the Fix type from Parser's Types, but that causes a cyclic import. 
  | EvdevSearchPrefix !FilePath !(NonEmpty FilePath)
  | EvdevSearchSuffix !FilePath !(NonEmpty FilePath)
  deriving Show

makeClassy ''EvdevCfg

getFile :: MonadIO m => EvdevCfg -> m (Maybe FilePath)
getFile = \case
  EvdevCfg fp -> pure . Just $ fp 
  EvdevSearchPrefix pre dirs -> findWith dirs (pre `L.isPrefixOf`) 
  EvdevSearchSuffix suf dirs -> findWith dirs (suf `L.isSuffixOf`)
  where
    findWith dirs f = filesIO dirs <&> firstCandidate f  
      where
        firstCandidate filt files = filter (view $ _2 . to filt) files ^? ix 0 . to mkPath 
        mkPath (dir, file) = dir <> "/" <> file 
        filesIO dirs = liftIO $ mconcat <$> mapM listPair (toList dirs)
        listPair dir = fmap (dir, ) <$> listDirectory dir

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

-- TODO: This is where the Mac config records go

-- | Placeholder
data KIOKitCfg = KIOKitCfg deriving Show

-- | Placeholder
data KextCfg = KextCfg deriving Show

--------------------------------------------------------------------------------
-- $cfgs-win

-- TODO: This is where the Win config records go

-- | Placeholder
data LLHookCfg = LLHookCfg deriving Show

-- | Placeholder
data SendEventCfg = SendEventCfg deriving Show

--------------------------------------------------------------------------------
-- $cfgs-sum
--
-- TODO: Here is where the sum-type of input and output configs go

data KeyInputCfg
  = LinuxEvdevCfg    EvdevCfg
  | MacKIOKitCfg     KIOKitCfg
  | WindowsLLHookCfg LLHookCfg
  deriving Show


data KeyOutputCfg
  = LinuxUinputCfg      UinputCfg
  | MacKextCfg          KextCfg
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
