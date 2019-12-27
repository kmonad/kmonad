module KMonad.Types.Message
  ( Message(..)
  , Port
  )
where

import KMonad.Prelude

import Data.Serialize
import Network.Socket

import KMonad.Types.Keyboard.Event


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
-- $server
--
-- Extra types to do with running the daemon server

type Port = ServiceName
