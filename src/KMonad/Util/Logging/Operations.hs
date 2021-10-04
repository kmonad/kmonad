module KMonad.Util.Logging.Operations
  ( dsp,
    ppRecord,
  )
where

import KMonad.Prelude
import qualified RIO.Text.Lazy as T (toStrict)
import Text.Pretty.Simple (pShowNoColor)

-- | Shorthand to pretty-print Displayable objects.
dsp :: Display a => a -> Text
dsp = textDisplay

-- | Pretty-print any showable object, useful for configuration records
ppRecord :: Show a => a -> Text
ppRecord = T.toStrict . pShowNoColor
