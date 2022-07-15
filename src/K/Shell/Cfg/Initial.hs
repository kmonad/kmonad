-- |

module K.Shell.Cfg.Initial
  ( -- * Basic types
    Label

    -- * Configuration data
    -- ** Configuration ADTs
    -- $vals
  , KeyInputCfg(..)
  , KeyOutputCfg(..)
  , KeyRepeatCfg(..)
  , LogColor(..)

    -- ** Configuration records
    -- $recs
  , RunCfg(..)
  , KioCfg(..)
  , LogCfg(..)
  , ShellCfg(..)

    -- * Lenses
    -- $lens
  , HasLogCfg(..)
  , HasKioCfg(..)
  , HasRunCfg(..)
  , HasShellCfg(..)


    -- * Reexports
  , module K.Shell.Initial
) where

import K.Shell.Initial

import qualified RIO.HashMap as M

-- basic types -----------------------------------------------------------------

-- | Label used to identify keycodes and gestures in a CfgFile
--
-- A valid label follows the following rules:
-- 1. starts with a letter or an underscore
-- 2. does not contain any whitespace
-- 3. semicolon is only allowed immediately after a backslash
--
-- Maybe this could be a newtype, but atm the boilerplate is not worth it
--
-- Note that all Dhall record keys are valid 'Label' (but not vice versa)
type Label = Text

-- config adts -----------------------------------------------------------------

-- | Different ways to read keyboard input from the world
data KeyInputCfg
  = LinEvdevSrc Path         -- ^ Linux evdev keyboard
  | WinHookSrc               -- ^ Windows low-level keyboard hook source
  | MacIOKitSrc (Maybe Text) -- ^ Mac IOKit keyboard source
  | CmdSrc Cmd               -- ^ Read stdout from some shell-command
  | StdinSrc                 -- ^ Read directly from stdin
  deriving (Eq, Show)

-- | Different ways to write keyboard output to the world
data KeyOutputCfg
  = LinUinputSnk (Maybe Text) -- ^ Linux uinput keyboard
  | WinSendSnk                -- ^ Windows SendEvent keyboard
  | MacKextSnk                -- ^ Mac Kext/Dext keyboard
  | CmdSnk Cmd                -- ^ Write to stdin of some shell-command
  | StdoutSnk                 -- ^ Write directly to stdout
  deriving (Eq, Show)

-- | What to do about key-repeat behavior
data KeyRepeatCfg
  = Simulate DelayRate -- ^ Internally simulate key-repeat process
  | EchoOS             -- ^ Use OS-provided repeat events to trigger key-repeats
  | IgnoreRepeat       -- ^ Don't model key-repeat at all
  deriving (Eq, Show)

-- | Different color schemes for logging output
data LogColor
  = LightBG    -- ^ Colors chosen with a light background in mind
  | DarkBG     -- ^ Colors chosen with a dark background in mind
  | Monochrome -- ^ No colors
  deriving (Eq, Show)

-- app cfg records -------------------------------------------------------------

-- | Settings that deal with task and permissions
data RunCfg = RunCfg
  { _cfgPath  :: Path     -- ^ Where to look for the .dhall configuration file
  , _kbdPath  :: Path     -- ^ Where to look for the .kbd keymap file
  , _cmdAllow :: CmdAllow -- ^ What style of command-execution policy to use
  , _task     :: Task     -- ^ What task to perform
  } deriving (Eq, Show)

-- | Settings that deal with keyboard IO
data KioCfg = KioCfg
  { _keyRepeatCfg :: KeyRepeatCfg  -- ^ How to handle key-repeat
  , _fallthrough  :: Bool          -- ^ Whether to reemit uncaught events
  , _keyInputCfg  :: KeyInputCfg   -- ^ How to acquire keyboard input
  , _keyOutputCfg :: KeyOutputCfg  -- ^ How to emit keyboard output
  , _preKioCmd    :: Cmd           -- ^ Command to run before initializing KeyIO
  , _postKioCmd   :: Cmd           -- ^ Command to run after initializing KeyIO
  } deriving (Eq, Show)

-- | Settings that deal with logging
data LogCfg = LogCfg
  { _logLevel  :: LogLevel -- ^ What level of log messages to display
  , _logColor  :: LogColor -- ^ Whether to use color for pretty-print
  , _useSep    :: Bool     -- ^ Whether to use log separators
  , _logTarget :: Handle   -- ^ Where to log to
  } deriving (Eq, Show)

-- | Collection of all configurations put together.
data ShellCfg = ShellCfg
  { _shellLocale :: Locale
  , _shellLogCfg :: LogCfg
  , _shellKioCfg :: KioCfg
  , _shellRunCfg :: RunCfg
  } deriving (Eq, Show)

-- lenses ----------------------------------------------------------------------

makeClassy ''RunCfg
makeClassy ''KioCfg
makeClassy ''LogCfg
makeClassy ''ShellCfg

instance HasLogCfg ShellCfg where logCfg = shellLogCfg
instance HasKioCfg ShellCfg where kioCfg = shellKioCfg
instance HasRunCfg ShellCfg where runCfg = shellRunCfg
instance HasLocale ShellCfg where locale = shellLocale
