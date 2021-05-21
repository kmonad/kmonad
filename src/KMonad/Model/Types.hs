module KMonad.Model.Types

where

import KMonad.Prelude
import KMonad.Util

import UnliftIO.STM

import qualified RIO.HashMap as R

--------------------------------------------------------------------------------
-- $basic
--
-- A collection of `simple` types used throughout the Model

-- | Layers are named simply by Text
type LayerName = Text

-- | A Layer maps keycodes to things
type Layer  a  = R.HashMap Keycode a

-- | A Keymap is an alist of named layers
--
-- NOTE: We use an AList, because we want to preserve ordering information from
-- the config. If no first-layer is specified, the first layer is assumed to be
-- the starting point of the map.
type Keymap a  = [(LayerName, (Layer a))]

--------------------------------------------------------------------------------
-- $api
--
-- The generalized interface through with a model interacts the IO-shell.
--

-- | The 'ModelAPI' record, containing the communication interface between App
-- and Model.
data ModelAPI = ModelAPI
  { _toModel :: TQueue KeyEvent -- ^ Events from shell to model
  , _toShell :: TQueue KeyEvent -- ^ Events from model to shell
  }
makeClassy ''ModelAPI

-- | Create a new 'ModelAPI'
mkModelAPI :: MonadIO m => m ModelAPI
mkModelAPI = ModelAPI <$> newTQueueIO <*> newTQueueIO

--------------------------------------------------------------------------------
-- $bcfg
--
-- Definitions of configurations for all the various buttons we support.
--

-- | Button ADT
data BCfg
  -- General
  = BEmit Keycode                     -- ^ Emit a keycode
  | BBlock                            -- ^ Button that catches event
  | BPause Ms                         -- ^ Pause for a period of time

  -- Layer manip
  | BLayerToggle Text                 -- ^ Toggle to a layer when held
  | BLayerSwitch Text                 -- ^ Switch base-layer when pressed
  | BLayerAdd Text                    -- ^ Add a layer when pressed
  | BLayerRem Text                    -- ^ Remove top instance of a layer when pressed
  | BLayerDelay Ms Name               -- ^ Switch to a layer for a period of time
  | BLayerNext Name                   -- ^ Perform next button in different layer

  -- Tap-hold functionality
  | BTapNext BCfg BCfg                -- ^ Do 2 things based on behavior
  | BTapHold Ms BCfg BCfg             -- ^ Do 2 things based on behavior and delay
  | BTapHoldNext Ms BCfg BCfg         -- ^ Mixture between KTapNext and KTapHold
  | BTapNextRelease BCfg BCfg         -- ^ Do 2 things based on behavior
  | BTapHoldNextRelease Ms BCfg BCfg  -- ^ Like KTapNextRelease but with a timeout

  -- Wrapping
  | BAround BCfg BCfg                 -- ^ Wrap 1 button around another
  | BAroundNext BCfg                  -- ^ Surround a future button
  | BAroundNextSingle BCfg            -- ^ Surround a future button

  -- Macros
  | BTapMacro [BCfg]                  -- ^ Sequence of buttons to tap
  | BTapMacroRelease [BCfg]           -- ^ Like KTapMacro, but tap the last when released

  -- Util
  | BMultiTap [(Ms, BCfg)] BCfg      -- ^ Do things depending on tap-count
  | BCommand Text (Maybe Text)       -- ^ Execute a shell command on press, as well as possibly on release
  | BStickyKey Ms BCfg               -- ^ Act as if a button is pressed for a period of time
 
  deriving Show

--------------------------------------------------------------------------------
-- $cfg
--
-- An implementation-independent representation of the desired KMonad behavior.
--

-- TODO: Move allowCmd entirely to the Shell, then change the ModelAPI to allow
-- the model to request shell-commands, but leave the implementation to the
-- Shell.

data ModelCfg = ModelCfg
  { _keymapCfg   :: Keymap BCfg -- ^ A 'Keymap' of button configurations
  , _firstLayer  :: LayerName   -- ^ What layer to start in
  , _fallThrough :: Bool        -- ^ Whether to let unmapped buttons fall through
  , _mAllowCmd   :: Bool        -- ^ Whether to allow shell-commands
  }
makeClassy ''ModelCfg
