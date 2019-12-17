{-# LANGUAGE DeriveAnyClass #-}
{-|
Module      : KMonad.Api.KeyIO.Types
Description : The interface between concrete IO and App data
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

This module defines an interface used to plug KeyIO functionality into App, and
KMonad. This means that if you want to extend KMonad by adding other key sources
or sinks, you must simple make sure your module provides a KeySource or KeySink
object that matches the types defined here, then KMonad will use this object to
interface with the OS.

-}
module KMonad.Api.KeyIO.Types
  ( -- * Common API to any type of OS keyboard interface
    -- $types
    KeySink
  , KeySource
  , BracketIO(..)
  , withKeySource
  , withKeySink
  , Emitter
  , Receiver
  , CanKeyIO

  --   -- * Wrapped KeySource that allows injecting events
  --   -- $esrc
  -- , EventSource
  -- , mkEventSource
  -- , readEvent
  -- , writeEvent

    -- * Collection of things that can go wrong with KeyIO
    -- $err
  , KeyIOError
  , AsKeyIOError(..)

    -- * Classy lenses and monad instances for KeyIO
  , HasEmitter(..)
  -- , HasEventSource(..)
  )
where

import Prelude hiding (read)

import Control.Exception (Exception, throw)
import Control.Lens
import Control.Monad.Except
import Data.Text (unpack)

import UnliftIO as U

import KMonad.Core



--------------------------------------------------------------------------------
-- $err

type SinkId = String

-- | The type of things that can go wrong with KeyIO
--
-- TODO: rewrap basic IO errors in a KeyIO error (stuff like "file not found")
data KeyIOError
  = IOCtlGrabError    FilePath
  | IOCtlReleaseError FilePath
  | EventParseError   FilePath String
  | SinkCreationError SinkId
  | SinkDeletionError SinkId
  | SinkWriteError    SinkId   KeyEvent
  deriving Exception

instance Show KeyIOError where
  show (IOCtlGrabError pth)
    = "Could not perform IOCTL grab on: " <> pth
  show (IOCtlReleaseError pth)
    = "Could not perform IOCTL release on: " <> pth
  show (EventParseError pth snk)
    = concat [ "Error parsing event when reading from "
             , pth
             , ": "
             , snk ]
  show (SinkCreationError snk)
    = "Error instantiating KeySink of type: " <> snk
  show (SinkDeletionError snk)
    = "Error deleting KeySink of type: " <> snk
  show (SinkWriteError snk e)
    = concat [ "Error writing event '"
             , unpack $ pretty e
             , "'to KeySink: "
             , snk ]

-- | Classy Prisms to create and throw 'KeyIOError's
makeClassyPrisms ''KeyIOError

instance AsKeyIOError IOException where
  _KeyIOError = prism' throw (const Nothing)

-- | They type of things that can perform KeyIO
type CanKeyIO e m = (AsKeyIOError e, MonadError e m, MonadIO m)


--------------------------------------------------------------------------------
-- $types

-- | The type of functions that emit key events to the OS
type Emitter = KeyEvent -> IO ()

-- | The type of actions that receive new key events
type Receiver = IO KeyEvent

-- | The generalized data type for IO actions that can only exist in the context
-- of being bracketed by some /acquire/ and /release/ behavior.
data BracketIO a = forall r. BracketIO
  { _open  :: IO r
  , _close :: r -> IO ()
  , _use   :: r -> a }

-- | Run an action that requires a managed /a/ by bracketting it with acquiring
-- and releasing the resource.
withBracketIO :: BracketIO a -> (a -> IO b) -> IO b
withBracketIO BracketIO{ _open=o, _close=c, _use=u } go
  = bracket o c (\a -> go $ u a)

-- | KeySink is a BracketIO specialized to a function that emits events
type KeySink   = BracketIO Emitter

-- | KeySource is a BracketIO specialized to an action that fetches events
type KeySource = BracketIO Receiver

-- | Run an action in the presence of an acquired KeySink
withKeySink :: KeySink -> (Emitter -> IO a) -> IO a
withKeySink = withBracketIO 

-- | Run an action in the presence of an acquired KeySource
withKeySource :: KeySource -> (Receiver -> IO a) -> IO a
withKeySource = withBracketIO

--------------------------------------------------------------------------------
-- $esrc
--
-- We cannot work with KeySource objects directly for awaiting 'Event's, as
-- 'srcRead' is a blocking call. Instead we wrap a 'KeySource' in an
-- 'EventSource', which manages input asynchronously, and allows waiting for
-- events from the keyboard while providing a mechanism to inject 'Event's into
-- KMonad's event-loop.

-- | An object that manages a KeySource and allows async injecting of other events
-- data EventSource = EventSource
--   { _waitKey :: Receiver
--   , _injectV :: MVar Event
--   }
-- -- | A classy lens to having an 'EventSource'
-- makeClassy ''EventSource

-- -- | Create a new EventSource by wrapping a receiver
-- mkEventSource :: MonadIO m => Receiver -> m EventSource
-- mkEventSource nextKey = EventSource nextKey <$> newEmptyMVar

-- -- | Await the next event from an EventSource, this blocks until an event occurs
-- readEvent :: MonadIO m => EventSource -> m Event
-- readEvent es =
--   liftIO $ U.race (InputEvent <$> es^.waitKey) (takeMVar $ es^.injectV) >>= \case
--     Left e  -> return e
--     Right e -> return e

-- -- | Inject a new event into the EventSource, can block if the events aren't
-- -- being handled fast enough.
-- writeEvent :: MonadIO m => Event -> EventSource -> m ()
-- writeEvent e es = liftIO . putMVar (es^.injectV) $ e



--------------------------------------------------------------------------------
-- $monad

-- | The property of having access to an Emitter function
class HasEmitter r where
  emitter :: Lens' r Emitter

