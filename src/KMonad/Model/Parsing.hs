{-|
Module      : KMonad.Model.Parsing
Description : The basic types of configuration parsing.
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

-}
module KMonad.Model.Parsing
  (
    -- * $but
    DefButton(..)
  , ImplArnd(..)

    -- * $tls
  , DefAlias
  , DefLayerSettings(..)
  , DefLayer(..)
  , DefSrc(..)
  , KExpr(..)

    -- * $defio
  , IToken(..)
  , OToken(..)

    -- * $lenses
  , AsKExpr(..)
  , HasDefSrc(..)
  , HasDefLayerSettings(..)
) where

import KMonad.Keyboard

--------------------------------------------------------------------------------
-- $but
--
-- Tokens representing different types of buttons

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
  | KTapNextRelease DefButton DefButton    -- ^ Do 2 things based on behavior
  | KTapHoldNextRelease Int DefButton DefButton (Maybe DefButton)
    -- ^ Like KTapNextRelease but with a timeout
  | KTapNextPress DefButton DefButton      -- ^ Like KTapNextRelease but also hold on presses
  | KTapHoldNextPress Int DefButton DefButton (Maybe DefButton)
    -- ^ Like KTapNextPress but with a timeout
  | KAroundNext DefButton                  -- ^ Surround a future button
  | KAroundNextSingle DefButton            -- ^ Surround a future button
  | KMultiTap [(Int, DefButton)] DefButton -- ^ Do things depending on tap-count
  | KStepped (NonEmpty DefButton)          -- ^ Do different things, one-by-one
  | KAround DefButton DefButton            -- ^ Wrap 1 button around another
  | KAroundOnly DefButton DefButton        -- ^ Wrap 1 button only around another
  | KAroundWhenAlone DefButton DefButton   -- ^ Wrap 1 button around another when it's "alone"
  | KAroundImplicit DefButton DefButton    -- ^ Wrap 1 button around another
  | KAroundNextTimeout Int DefButton DefButton
  | KTapMacro (NonEmpty DefButton) (Maybe Int)
    -- ^ Sequence of buttons to tap, possible delay between each press
  | KTapMacroRelease (NonEmpty DefButton) (Maybe Int)
    -- ^ Sequence of buttons to tap, tap last on release, possible delay between each press
  | KComposeSeq (NonEmpty DefButton)       -- ^ Compose-key sequence
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
-- $tls
--
-- A collection of all the different top-level statements possible in a config
-- file.

-- | A list of keycodes describing the ordering used by all other layers
-- | which is associated with a name.
data DefSrc = DefSrc
  { _srcName  :: Maybe Text -- ^ A unique name used to refer to this layer.
  , _keycodes :: [Keycode]  -- ^ Layer settings containing also the buttons.
  }
  deriving (Show, Eq)
makeClassy ''DefSrc

-- | A mapping from names to button tokens
type DefAlias = [(Text, DefButton)]

data DefLayerSettings = DefLayerSettings
  { _lSrcName  :: [Text]
  , _lImplArnd :: [ImplArnd]
  , _lButtons  :: [DefButton]
  }
  deriving (Show, Eq)
makeClassy ''DefLayerSettings
instance Semigroup DefLayerSettings where
  DefLayerSettings sn ia bs <> DefLayerSettings sn' ia' bs' =
    DefLayerSettings (sn ++ sn') (ia ++ ia') (bs ++ bs')
instance Monoid DefLayerSettings where
  mempty = DefLayerSettings [] [] []

-- | A layer of buttons
data DefLayer = DefLayer
  { _layerName :: Text
  , _layerSettings :: DefLayerSettings
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
  deriving (Show, Eq)

-- | All different output-tokens KMonad can take
data OToken
  = KUinputSink Text (Maybe Text)
  | KSendEventSink (Maybe (Int, Int))
  | KKextSink
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- $tkn

-- | Any statement in a config-file must parse to a 'KExpr'
data KExpr
  = KDefSrc   DefSrc
  | KDefLayer DefLayer
  | KDefAlias DefAlias
  deriving (Show, Eq)
makeClassyPrisms ''KExpr
