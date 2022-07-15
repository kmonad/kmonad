-- |

module K.Layers.Initial
  ( KbdFile(..)

  , LayerName
  , ButtonName
  , SrcName

  , But(..)
  , Layer(..)
  , KeymapCfg(..)

  , module K.Initial
  )

where

import K.Initial
import K.Keyboard
import qualified RIO.HashMap as M

type LayerName = Text  -- ^ Refer to layer by its name
type ButtonName = Text -- ^ Refer to aliased button
type SrcName = Text    -- ^ Refer to specific src layer

-- | Button ADT
--
-- NOTE: Here 'c' is the type of key-identifier we use. The first parsing run of
-- a kbd-file will use 'But Keyname', then we use the locale to fmap that into
-- 'But Keycode'
data But c
  = BRef ButtonName
  -- ^ Reference a named button
  | BEmit c
  -- ^ Emit a keycode
  | BPressOnly c
  -- ^ Emit only the press of a keycode
  | BReleaseOnly c
  -- ^ Emit only the release of a keycode
  | BLayerToggle LayerName
  -- ^ Toggle to a layer when held
  | BLayerSwitch LayerName
  -- ^ Switch base-layer when pressed
  | BLayerAdd LayerName
  -- ^ Add a layer when pressed
  | BLayerRem LayerName
  -- ^ Remove top instance of a layer when pressed
  | BTapNext (But c) (But c)
  -- ^ Do 2 things based on behavior
  | BTapHold Dt (But c) (But c)
  -- ^ Do 2 things based on behavior and delay
  | BTapHoldNext Dt (But c) (But c) (Maybe (But c))
    -- ^ Mixture between BTapNext and BTapHold
  | BTapNextRelease (But c) (But c)
  -- ^ Do 2 things based on behavior
  | BTapHoldNextRelease Dt (But c) (But c) (Maybe (But c))
    -- ^ Like BTapNextRelease but with a timeout
  | BAroundNext (But c)
  -- ^ Surround a future button
  | BAroundNextSingle (But c)
  -- ^ Surround a future button
  | BMultiTap [(Dt, But c)] (But c)
  -- ^ Do things depending on tap-count
  | BAround (But c) (But c)
  -- ^ Wrap 1 button around another
  | BAroundNextTimeout Dt (But c) (But c)
  -- ^ Wrap 1 button around another, but timeout after a while
  | BTapMacro [But c] (Maybe Dt)
    -- ^ Sequence of buttons to tap, possible delay between each press
  | BTapMacroRelease [But c] (Maybe Dt)
    -- ^ Sequence of buttons to tap, tap last on release, possible delay between each press
  | BComposeSeq [But c]
  -- ^ Compose-key sequence
  | BPause Dt
  -- ^ Pause for a period of time
  | BLayerDelay Dt LayerName
  -- ^ Switch to a layer for a period of time
  | BLayerNext LayerName
  -- ^ Perform next button in different layer
  | BCommand Text (Maybe Text)
  -- ^ Execute a shell command on press, as well as possibly on release
  | BBeforeAfterNext (But c) (But c)
  -- ^ Peform a tap before and after the following button
  | BStickyKey Dt (But c)
  -- ^ Act as if a button is pressed for a period of time
  | BTrans
  -- ^ Transparent button that does nothing
  | BBlock
  -- ^ Button that catches event
  deriving (Eq, Show, Functor)



data KbdFile = KbdFile

newtype Layer c = Layer { _layer :: [(c, But c)]}
  deriving (Eq, Show, Functor)

data KeymapCfg = KeymapCfg
  { _layers  :: Named (Layer Keycode)
  , _aliases :: Named (But Keycode)
  }
