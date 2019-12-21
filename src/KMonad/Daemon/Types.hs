module KMonad.Daemon.Types

where

import KMonad.Prelude

import KMonad.Types.Message
import KMonad.Types.Keyboard.IO
import KMonad.Types.Keymap


--------------------------------------------------------------------------------




-- | The 'MapCfg' describes how to load the keymap
data MapCfg
  = LoadKeyMapFromFile !FilePath -- ^ Read and parse a file
  | ParseFromText      !Text     -- ^ Parse 'Text' directly
  | TakeDirectly       !KeyMap   -- ^ Use `precompiled` KeyMap

-- | The 'DaemonCfg' describes the configuration of a the KMonad daemon
data DaemonCfg = DaemonCfg
  { _kcOpenKeySource :: !(Acquire KeySource)
  , _kcOpenKeySink   :: !(Acquire KeySink)
  , _kcPort          :: !Port
  , _kcMapCfg        :: !MapCfg
  }

makeClassy ''MapCfg
makeLenses ''DaemonCfg

class HasMapCfg e => HasDaemonCfg e where
  daemonCfg :: Lens' e DaemonCfg

  openKeySource :: Lens' e (Acquire KeySource)
  openKeySource = daemonCfg . kcOpenKeySource

  openKeySink :: Lens' e (Acquire KeySink)
  openKeySink = daemonCfg . kcOpenKeySink

instance HasMapCfg    DaemonCfg where mapCfg    = kcMapCfg
instance HasPort      DaemonCfg where port      = kcPort
instance HasDaemonCfg DaemonCfg where daemonCfg = id



