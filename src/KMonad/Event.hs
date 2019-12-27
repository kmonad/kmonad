module KMonad.Event

where

import KMonad.Prelude

import KMonad.Keyboard
import KMonad.Util

type KeyEvent = Timed KeyAction
type MsgEvent = Timed Message


--------------------------------------------------------------------------------
-- $msg
--
-- 'Message's are the collection of messages that can be sent to the KMonad
-- daemon through its messaging socket.

-- | The different 'Message's that can be sent to KMonad
data Message
  = Shutdown           -- ^ Signal to quit
  | EmitKey !KeyAction -- ^ Signal to emit a key action to the OS
  deriving (Eq, Show, Generic)

instance Serialize Message


--------------------------------------------------------------------------------
-- $event
--
-- 'Event's are the collection of all things that can trigger KMonad to update.

-- | 'Event' is the sum of all events that can be given to KMonad
data Event
  = KIOEvent     !KeyEvent -- ^ A 'KeyEvent' is registered from the 'KeySource'
  | MessageEvent !MsgEvent -- ^ A 'Message' is received through the network socket
  | Quit                   -- ^ A 'Quit' event is sent from somewhere inside KMonad
  deriving (Eq, Show, Generic)

instance Serialize Event
