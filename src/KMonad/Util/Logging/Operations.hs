module KMonad.Util.Logging.Operations
  ( dsp
  )
where

import KMonad.Prelude

-- | Shorthand to pretty-print Displayable objects.
dsp :: Display a => a -> Text
dsp = textDisplay
