{-|
Module      : KMonad.Args.Types
Description : The basic types of configuration parsing.
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

-}
module KMonad.Args.Types
  (
    -- * $cfg
    CfgToken(..)

    -- * $but
  , DefButton(..)
  , ImplArnd(..)

    -- * $tls
  , DefSetting(..)
  , DefSettings
  , DefAlias
  , DefLayerSetting(..)
  , DefLayer(..)
  , DefSrc(..)
  , KExpr(..)

    -- * $defio
  , IToken(..)
  , OToken(..)

    -- * $lenses
  , AsKExpr(..)
  , AsDefSetting(..)
  , HasDefSrc(..)
  , AsDefLayerSetting(..)
) where


import KMonad.Prelude

import KMonad.Model.Button
import KMonad.Keyboard
import KMonad.Keyboard.IO
import KMonad.Util

--------------------------------------------------------------------------------
-- $but
--
-- Tokens representing different types of buttons

-- FIXME: This is really broken: why are there 2 lists of 'DefButton's? There is
-- one here, and one in Parser/Types.hs

-- | Button ADT
data DefButton
  = KRef Text                              -- ^ Reference a named button
  | KEmit Keycode                          -- ^ Emit a keycode
  | KPressOnly Keycode                     -- ^ Emit only the press of a keycode
  | KReleaseOnly Keycode                   -- ^ Emit only the release of a keycode
  | KLayerToggle Text                      -- ^ Toggle to a layer when held
  | KLayerSwitch Text                      -- ^ Switch base-layer when pressed
  | KLayerAdd Text                         -- ^ Add a layer when pressed
  | KLayerRem Text                         -- ^ Remove top instance of a layer when pressed
  | KTapNext DefButton DefButton           -- ^ Do 2 things based on behavior
  | KTapHold Int DefButton DefButton       -- ^ Do 2 things based on behavior and delay
  | KTapHoldNext Int DefButton DefButton (Maybe DefButton)
    -- ^ Mixture between KTapNext and KTapHold
  | KTapNextRelease (Maybe Int) DefButton DefButton    -- ^ Do 2 things based on behavior
  | KTapHoldNextRelease Int (Maybe Int) DefButton DefButton (Maybe DefButton)
    -- ^ Like KTapNextRelease but with a timeout
  | KTapNextPress DefButton DefButton      -- ^ Like KTapNextRelease but also hold on presses
  | KTapHoldNextPress Int DefButton DefButton (Maybe DefButton)
    -- ^ Like KTapNextPress but with a timeout
  | KAroundNext DefButton                  -- ^ Surround a future button
  | KAroundNextSingle DefButton            -- ^ Surround a future button
  | KMultiTap [(Int, DefButton)] DefButton -- ^ Do things depending on tap-count
  | KStepped [DefButton]                   -- ^ Do different things, one-by-one
  | KAround DefButton DefButton            -- ^ Wrap 1 button around another
  | KAroundOnly DefButton DefButton        -- ^ Wrap 1 button only around another
  | KAroundWhenAlone DefButton DefButton   -- ^ Wrap 1 button around another when it's "alone"
  | KAroundImplicit DefButton DefButton    -- ^ Wrap 1 button around another
  | KAroundNextTimeout Int DefButton DefButton
  | KTapMacro [DefButton] (Maybe Int)
    -- ^ Sequence of buttons to tap, possible delay between each press
  | KTapMacroRelease [DefButton] (Maybe Int)
    -- ^ Sequence of buttons to tap, tap last on release, possible delay between each press
  | KComposeSeq [DefButton]                -- ^ Compose-key sequence
  | KPause Milliseconds                    -- ^ Pause for a period of time
  | KLayerDelay Int LayerTag               -- ^ Switch to a layer for a period of time
  | KLayerNext LayerTag                    -- ^ Perform next button in different layer
  | KCommand Text (Maybe Text)             -- ^ Execute a shell command on press, as well
                                           --   as possibly on release
  | KStickyKey Int DefButton               -- ^ Act as if a button is pressed for a period of time
  | KBeforeAfterNext DefButton DefButton   -- ^ Surround a future button in a before and after tap
  | KTrans                                 -- ^ Transparent button that does nothing
  | KBlock                                 -- ^ Button that catches event
  deriving (Show, Eq, Typeable, Data)

instance Plated DefButton

-- | Possible values for implicit around
data ImplArnd
  = IADisabled
  | IAAround
  | IAAroundOnly
  | IAAroundWhenAlone
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- $cfg
--
-- The Cfg token that can be extracted from a config-text without ever entering
-- IO. This will then directly be translated to a DaemonCfg
--

-- | The 'CfgToken' contains all the data needed to construct an
-- 'KMonad.App.AppCfg'.
data CfgToken = CfgToken
  { _src   :: LogFunc -> IO (Acquire KeySource) -- ^ How to grab the source keyboard
  , _snk   :: LogFunc -> IO (Acquire KeySink)   -- ^ How to construct the out keybboard
  , _km    :: LMap Button                       -- ^ An 'LMap' of 'Button' actions
  , _fstL  :: LayerTag                          -- ^ Name of initial layer
  , _flt   :: Bool                              -- ^ How to deal with unhandled events
  , _allow :: Bool                              -- ^ Whether to allow shell commands
  , _ksd   :: Maybe Int                         -- ^ Output delay between keys
  }
makeClassy ''CfgToken


--------------------------------------------------------------------------------
-- $tls
--
-- A collection of all the different top-level statements possible in a config
-- file.

-- | A list of keycodes describing the ordering used by all other layers
-- | which is associated with a name.
data DefSrc = DefSrc
  { _srcName  :: Maybe Text   -- ^ A unique name used to refer to this layer.
  , _keycodes :: [DefButton]  -- ^ Layer settings containing also the buttons.
  }
  deriving (Show, Eq)
makeClassy ''DefSrc

-- | A mapping from names to button tokens
type DefAlias = [(Text, DefButton)]

data DefLayerSetting
  = LSrcName Text
  | LImplArnd ImplArnd
  | LButton DefButton
  deriving (Show, Eq)
makeClassyPrisms ''DefLayerSetting

-- | A layer of buttons
data DefLayer = DefLayer
  { _layerName :: Text
  , _layerSettings :: [DefLayerSetting]
  }
  deriving (Show, Eq)


--------------------------------------------------------------------------------
-- $defcfg
--
-- Different settings

-- | All different input-tokens KMonad can take
data IToken
  = KDeviceSource FilePath
  | KLowLevelHookSource
  | KIOKitSource (Maybe Text)
  deriving (Show)

-- | All different output-tokens KMonad can take
data OToken
  = KUinputSink Text (Maybe Text)
  | KSendEventSink (Maybe (Int, Int))
  | KKextSink
  deriving (Show)

-- | All possible single settings
data DefSetting
  = SIToken      IToken
  | SOToken      OToken
  | SCmpSeq      DefButton
  | SFallThrough Bool
  | SAllowCmd    Bool
  | SCmpSeqDelay Int
  | SKeySeqDelay Int
  | SImplArnd    ImplArnd
  deriving (Show)
makeClassyPrisms ''DefSetting

-- | 'Eq' instance for a 'DefSetting'. Because every one of these options may be
-- given at most once, we only need to check the outermost constructor in order
-- to test for equality
instance Eq DefSetting where
  SIToken{}      == SIToken{}      = True
  SOToken{}      == SOToken{}      = True
  SCmpSeq{}      == SCmpSeq{}      = True
  SFallThrough{} == SFallThrough{} = True
  SAllowCmd{}    == SAllowCmd{}    = True
  SImplArnd{}    == SImplArnd{}    = True
  SCmpSeqDelay{} == SCmpSeqDelay{} = True
  SKeySeqDelay{} == SKeySeqDelay{} = True
  _              == _              = False

-- | A list of different 'DefSetting' values
type DefSettings = [DefSetting]

--------------------------------------------------------------------------------
-- $tkn

-- | Any statement in a config-file must parse to a 'KExpr'
data KExpr
  = KDefCfg   DefSettings
  | KDefSrc   DefSrc
  | KDefLayer DefLayer
  | KDefAlias DefAlias
  deriving (Show, Eq)
makeClassyPrisms ''KExpr


--------------------------------------------------------------------------------
-- $act
