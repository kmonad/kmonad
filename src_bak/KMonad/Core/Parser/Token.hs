{-|
Module      : KMonad.Core.Parser.Token
Description : The different tokens that can be read from config files.
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

We deal with reading configuration files by first parsing the entire file into a
collection of abstract data tokens. It is only in the second pass when we
interpret these data-tokens into an actual 'Kmonad.Core.Config.Config' object
that we check for correctness of the config.

This module contains all of the definitions of things that can be parsed
straight from text, in addition to KeyCode's.

-}
module KMonad.Core.Parser.Token
  ( Symbol
  , InputDecoder(..)
  , InputToken(..)
  , OutputToken(..)
  , ButtonToken(..)
  , ButtonSymbol(..)
  , SourceToken(..)
  , AliasRef(..)
  , AliasDef(..)
  , LayerToken(..)
  , layerName, anchor, buttons
  , ConfigToken(..)
  , sources, layers, aliases, inputs, outputs
  )
where

import Control.Lens
import Data.Text

import KMonad.Core.KeyCode
import KMonad.Core.Keyboard
import KMonad.Core.Matrix
import KMonad.Core.SpecialSymbol
import KMonad.Core.Time
import KMonad.Core.Types


--------------------------------------------------------------------------------

-- TODO: Move Name somewhere sane

-- | The type of 'Symbol' keywords used in our mini-language.
type Symbol = Text

--------------------------------------------------------------------------------

-- | A token describing which input-decoder to use
data InputDecoder
  = L64   -- ^ The standard, Linux 64-bit representation of key events
  deriving (Eq, Show)

-- | A token describing which input IO to use
data InputToken
  = LinuxDeviceSource InputDecoder FilePath -- ^ The standard, Linux 64-bit input device
  | WindowsLLHook                           -- ^ The initial, simple Windows hook
  deriving (Eq, Show)

-- | A token describing which output to use
data OutputToken
  = UinputDevice (Maybe Text) (Maybe Text) -- ^ A Linux uinput device
  | WindowsSendEventSink                   -- ^ API using windows SendEvent calls internally
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | A token describing a button
data ButtonToken
  = BAfter ButtonToken ButtonToken
  | BEmit KeyCode
  | BModded KeyCode ButtonToken
  | BLayerToggle Name
  | BTapHold Milliseconds ButtonToken ButtonToken
  | BTapNext ButtonToken ButtonToken
  | BMacro KeySequence
  | BMultiTap [(Microseconds, ButtonToken)]
  | BBlock
  | BLayerAdd Name
  | BLayerRem Name
  | BLockOn LockKey
  | BLockOff LockKey
  | BLockToggle LockKey
  | BEmitSpecial SpecialSymbol
  | BEmitDeadKey DeadKey
  deriving (Eq, Ord, Show)

-- | A token describing anything that can be interpreted as a 'ButtonToken'
data ButtonSymbol
  = BSToken ButtonToken -- ^ A literal 'ButtonToken'
  | BSAlias AliasRef    -- ^ A named alias, to be dereferenced later
  | Transparent         -- ^ Indicate that this keycode is not handled by the current layer
  deriving (Eq, Show)


--------------------------------------------------------------------------------

-- | A token representing the layout of 'KeyCode's that we are mapping all other
-- layers onto.
data SourceToken = SourceToken (Matrix KeyCode)
  deriving (Eq, Show)


--------------------------------------------------------------------------------

-- | A token representing the act of referring to a previously defined 'AliasDef'
data AliasRef = AliasRef Symbol             deriving (Eq, Show)

-- | A token representing the definition of an alias as correspondence between a
-- 'Symbol' and a 'ButtonToken'.
data AliasDef = AliasDef Symbol ButtonToken deriving (Eq, Show)


--------------------------------------------------------------------------------

-- | A token representing an entire layer of buttons
data LayerToken = LayerToken
  { _layerName :: Name             -- ^ The name of this layer
  , _anchor    :: Maybe KeyCode       -- ^ Where to anchor this layer to the source layer
  , _buttons   :: Matrix ButtonSymbol -- ^ A matrix of 'ButtonSymbol's to map to the source
  } deriving (Eq, Show)

-- | Provide some classy lenses for ease of use
makeClassy ''LayerToken


--------------------------------------------------------------------------------

-- | A token representing the entire parse of a configuration file. Note that
-- this does not necessarily correspond to a valid config. The conversion from a
-- 'ConfigToken' to a 'KMonad.Core.Config.Config' is done by
-- 'KMonad.Core.Config.interpret'.
data ConfigToken = ConfigToken
  { _sources :: [SourceToken] -- ^ A list of all the 'SourceToken' encountered
  , _layers  :: [LayerToken]  -- ^ A list of all the 'LayerToken' encountered
  , _aliases :: [AliasDef]    -- ^ A list of all the 'AliasDef' encountered
  , _inputs  :: [InputToken]  -- ^ A list of all the 'InputToken' encountered
  , _outputs :: [OutputToken] -- ^ A list of all the 'OutputToken' encountered
  } deriving (Eq, Show)

-- | Provide some classy lenses for ease of use
makeClassy ''ConfigToken
