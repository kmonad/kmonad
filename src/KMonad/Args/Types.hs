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
  ( -- * $bsc
    Parser
  , PErrors(..)

    -- * $cfg
  , CfgToken(..)

    -- * $but
  , DefButton(..)

    -- * $tls
  , DefSetting(..)
  , DefSettings
  , DefAlias
  , DefLayer(..)
  , DefSrc
  , KExpr(..)

    -- * $defio
  , IToken(..)
  , OToken(..)

    -- * $lenses
  , AsKExpr(..)
  , AsDefSetting(..)

    -- * Reexports
  , module Text.Megaparsec
  , module Text.Megaparsec.Char
) where


import KMonad.Prelude

import KMonad.Model.Button
import KMonad.Keyboard
import KMonad.Keyboard.IO
import KMonad.Util

import Text.Megaparsec
import Text.Megaparsec.Char

--------------------------------------------------------------------------------
-- $bsc
--
-- The basic types of parsing

-- | Parser's operate on Text and carry no state
type Parser = Parsec Void Text

-- | The type of errors returned by the Megaparsec parsers
newtype PErrors = PErrors (ParseErrorBundle Text Void)

instance Show PErrors where
  show (PErrors e) = "Parse error at " <> errorBundlePretty e

instance Exception PErrors

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
  | KLayerToggle Text                      -- ^ Toggle to a layer when held
  | KLayerSwitch Text                      -- ^ Switch base-layer when pressed
  | KLayerAdd Text                         -- ^ Add a layer when pressed
  | KLayerRem Text                         -- ^ Remove top instance of a layer when pressed
  | KTapNext DefButton DefButton           -- ^ Do 2 things based on behavior
  | KTapHold Int DefButton DefButton       -- ^ Do 2 things based on behavior and delay
  | KTapHoldNext Int DefButton DefButton   -- ^ Mixture between KTapNext and KTapHold
  | KTapNextRelease DefButton DefButton    -- ^ Do 2 things based on behavior
  | KTapHoldNextRelease Int DefButton DefButton (Maybe DefButton)
    -- ^ Like KTapNextRelease but with a timeout
  | KAroundNext DefButton                  -- ^ Surround a future button
  | KAroundNextSingle DefButton            -- ^ Surround a future button
  | KMultiTap [(Int, DefButton)] DefButton -- ^ Do things depending on tap-count
  | KAround DefButton DefButton            -- ^ Wrap 1 button around another
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
  | KTrans                                 -- ^ Transparent button that does nothing
  | KBlock                                 -- ^ Button that catches event
  deriving Show


--------------------------------------------------------------------------------
-- $cfg
--
-- The Cfg token that can be extracted from a config-text without ever enterring
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
  }
makeClassy ''CfgToken


--------------------------------------------------------------------------------
-- $tls
--
-- A collection of all the different top-level statements possible in a config
-- file.

-- | A list of keycodes describing the ordering of all the other layers
type DefSrc = [Keycode]

-- | A mapping from names to button tokens
type DefAlias = [(Text, DefButton)]

-- | A layer of buttons
data DefLayer = DefLayer
  { _layerName :: Text        -- ^ A unique name used to refer to this layer
  , _buttons   :: [DefButton] -- ^ A list of button tokens
  }
  deriving Show


--------------------------------------------------------------------------------
-- $defcfg
--
-- Different settings

-- | All different input-tokens KMonad can take
data IToken
  = KDeviceSource FilePath
  | KLowLevelHookSource
  | KIOKitSource (Maybe Text)
  deriving Show

-- | All different output-tokens KMonad can take
data OToken
  = KUinputSink Text (Maybe Text)
  | KSendEventSink
  | KKextSink
  deriving Show

-- | All possible single settings
data DefSetting
  = SIToken      IToken
  | SOToken      OToken
  | SCmpSeq      DefButton
  | SInitStr     Text
  | SFallThrough Bool
  | SAllowCmd    Bool
  | SCmpSeqDelay Int
  deriving Show
makeClassyPrisms ''DefSetting

-- | 'Eq' instance for a 'DefSetting'. Because every one of these options may be
-- given at most once, we only need to check the outermost constructor in order
-- to test for equality
instance Eq DefSetting where
  SIToken{}      == SIToken{}      = True
  SOToken{}      == SOToken{}      = True
  SCmpSeq{}      == SCmpSeq{}      = True
  SInitStr{}     == SInitStr{}     = True
  SFallThrough{} == SFallThrough{} = True
  SAllowCmd{}    == SAllowCmd{}    = True
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
  deriving Show
makeClassyPrisms ''KExpr


--------------------------------------------------------------------------------
-- $act
