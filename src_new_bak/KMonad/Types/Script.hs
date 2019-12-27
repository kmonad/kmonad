module KMonad.Types.Script

where

import KMonad.Prelude

-- | How to provide KeyIO to the launched script
data KIOMode
  = NoInput    -- ^ Not at all
  | CopyKIO    -- ^ Send a copy of the input stream
  | CaptureKIO -- ^ Capture KIO, pausing KMonad until script releases KIO
  deriving (Eq, Show)

data ScriptCfg = ScriptCfg
  { _waitForComplete :: !Bool    -- ^ Whether to pause this thread until the script finishes
  , _scriptCmd       :: !Text    -- ^ The command to run
  , _kioMode         :: !KIOMode -- ^ How to provide KeyIO to the script
  }
