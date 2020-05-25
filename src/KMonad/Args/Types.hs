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

import KMonad.Button
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

-- | Button ADT
data DefButton
  = KRef Text                              -- ^ Reference a named button
  | KEmit Keycode                          -- ^ Emit a keycode
  | KLayerToggle Text                      -- ^ Toggle to a layer when held
  | KLayerSwitch Text                      -- ^ Switch base-layer when pressed
  | KTapNext DefButton DefButton           -- ^ Do 2 things based on behavior
  | KTapHold Int DefButton DefButton       -- ^ Do 2 things based on behavior and delay
  | KAroundNext DefButton                  -- ^ Surround a future button
  | KMultiTap [(Int, DefButton)] DefButton -- ^ Do things depending on tap-count
  | KAround DefButton DefButton            -- ^ Wrap 1 button around another
  | KTapMacro [DefButton]                  -- ^ Sequence of buttons to tap
  | KComposeSeq [DefButton]                -- ^ Compose-key sequence
  | KPause Milliseconds                    -- ^ Pause for a period of time
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
  { _src  :: LogFunc -> IO (Acquire KeySource) -- ^ How to grab the source keyboard
  , _snk  :: LogFunc -> IO (Acquire KeySink)   -- ^ How to construct the out keybboard
  , _km   :: LMap Button                       -- ^ An 'LMap' of 'Button' actions
  , _fstL :: LayerTag                          -- ^ Name of initial layer
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
  deriving Show

-- | All different output-tokens KMonad can take
data OToken
  = KUinputSink Text (Maybe Text)
  | KSendEventSink
  deriving Show

-- | All possible single settings
data DefSetting
  = SIToken  IToken
  | SOToken  OToken
  | SCmpSeq  DefButton
  | SInitStr Text
  deriving Show
makeClassyPrisms ''DefSetting

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
