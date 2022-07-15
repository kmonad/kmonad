-- |

module K.Keyboard.Initial
  ( Keycode
  , Keyname
  , Rap

  , module K.Initial
  , module K.Gesture
  )

where

import K.Initial
import K.Gesture

-- basic types -----------------------------------------------------------------

-- | Keycodes are represented as a natural number
type Keycode = Natural

-- | Keynames are simple names
type Keyname = Name

-- | A gesture on the keyboard
--
-- NOTE: 'Rap' as in: /rapping at my chamber-door/, as in: /eazy-e was rapping,
-- rapping at my chamber door/... Should there be bad-jokes in documentation?
-- First to find it gets to remove it.
type Rap = Gesture Keycode
