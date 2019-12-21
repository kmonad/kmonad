module KMonad.Types.Event
  ()
where

import KMonad.Prelude

import Data.Serialize

import KMonad.Types.Keyboard.Event
import KMonad.Types.Message

--------------------------------------------------------------------------------
-- $event
--
-- 'Event's are the collection of all things that can trigger KMonad to update.

-- | 'Event' is the sum of all events that can be given to KMonad
data Event
  = KIOEvent !KeyEvent    -- ^ A 'KeyEvent' is registered from the 'KeySource'
  | MessageEvent !Message -- ^ A 'Message' is received through the network socket
  | Quit                  -- ^ A 'Quit' event is sent from somewhere inside KMonad
  deriving (Eq, Show, Generic)

instance Serialize Event
