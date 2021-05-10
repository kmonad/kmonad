{- The KeyIO types common to all OSes

NOTE: This is where the configuration records for all KeyIO functionality should
live. I.e. even on Linux we should be able to parse MacOS configurations and
vice-versa. The function that dispatches the configuration to the actual loading
of a keyboard will fail, but we should be able to construct a configuration.
This way we can specify multiple keyboard configurations in the same file, and
then just load the one for the OS that is currently running KMonad.

-}
module KMonad.KeyIO.Common.Types

where

import KMonad.Prelude
import KMonad.Util.Name
import KMonad.Types


import Control.Exception.Lens

--------------------------------------------------------------------------------
-- $result
--
-- Once all the configuration and initialization is done, this is what the
-- actual _kmonad engine should end up with: an IO action that gets keycodes,
-- and an IO action that writes keycodes somewhere. Once we are up and running,
-- there is no more distinction between OSes or production-vs-testing.
--
-- NOTE: This is not a very common type, it only exists to point at the fact
-- that any OS-specific implementation must define some 'Keycode' type and then
-- plug it into these types. 'GetKey' and 'PutKey' are the actually /completed/
-- types that we work with.

type CanKeycode c = (Eq c, Ord c, Num c, Show c, Enum c, Hashable c)

type GetKey_ c = IO c
type PutKey_ c = (c -> IO ())

--------------------------------------------------------------------------------
-- $linux-cfg


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

-- | The configuration record for evdev key-input
data EvdevCfg = EvdevCfg
  { _pth :: FilePath -- ^ The path to the input-file to open and capture
  }
makeClassy ''EvdevCfg

--------------------------------------------------------------------------------
-- $tokens
--
-- KeyIO tokens are descriptive: they point at what type of KeyIO needs to be
-- done, and what configuration should be passed to the chosen key-io
-- functionality. We separate getting from putting. Furthermore, we separate
-- OS-specific real-world IO from cross-platform testing IO.
--
-- NOTE: The configuration records for an OS-specific KeyIO module must be
-- provided in 'Common', for example: 'EvdevCfg' describes exactly what keyboard
-- to open with the evdev module, and is available on every OS. The way that
-- this record gets handled by the engine differs (it only works on Linux), but
-- the configuration can be parsed and represented on any OS. This way we don't
-- have to do any OS-specific parsing of configuration-files, and people can use
-- 1 config file across different OSes.

-- | ADT describing all possible testing inputs
data TestGetCfg
  = ListGet Text -- ^ A list of events to-be-parsed-from Text

-- | ADT describing all possible testing outputs
data TestPutCfg
  = LogPut       -- ^ Test output that logs events according to LogEnv

-- | ADT describing all possible input configurations
data IOGetCfg
  = EvdevGet EvdevCfg -- ^ Use a linux evdev file with ioctl

-- | ADT describing all possible output configurations
data IOPutCfg
  = UinputPut UinputCfg -- ^ Uses linux 'uinput' kernel module to inject events

type GetCfg = Either TestGetCfg IOGetCfg
type PutCfg = Either TestPutCfg IOPutCfg

--------------------------------------------------------------------------------

-- | IsPress is a boolean value where True indicates a press, and False
-- indicates a release. We simply try to ameliorate some of the boolean
-- blindness by naming the type.
type IsPress = Bool

--------------------------------------------------------------------------------
-- $exc
--

-- | The things that can go wrong with KeyIO. I'd like to have more informative
-- packets than 'Text', but the environments and events are different between
-- OSes, and we basically never try to recover from these exceptions using the
-- information packets that they wrap.
data KioException
  = CouldNotAcquireKio  Text -- ^ Problem trying to acquire resource
  | CouldNotReleaseKio  Text -- ^ Problem trying to relaese resource
  | CouldNotEncodeEvent Text -- ^ Problem encoding an event to be emitted
  | CouldNotDecodeEvent Text -- ^ Problem decoding event from OS
  | KioResourceLost     Text -- ^ Already acquired resource lost
  deriving Show
makeClassyPrisms ''KioException


instance Exception KioException
instance AsKioException SomeException where _KioException = exception


