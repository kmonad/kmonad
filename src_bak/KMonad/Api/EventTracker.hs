{-|
Module      : KMonad.Api.EventTracker
Description : Functionality to register and transmit EventComparison requests.
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

For certain button functionality, we need to know what will happen in the future
before we decide what action to take. For example, a
'KMonad.Core.Parser.Token.BTapHold' button decides to become a "tapper" if it
encounters its own release within a certain interval, and otherwise switches to
a "held" state. 'Button's, however, are isolated from knowing what 'KeyEvent'
triggers them. But they can get access to 'EventComparison' data through the
'KMonad.Domain.Effect.Future.MonadFuture' typeclass.

The 'EventTracker' concretely implements how to provide this functionality using
IO.

-}
module KMonad.Api.EventTracker
  ( EventTracker
  , HasEventTracker
  , eventTracker
  , mkEventTracker
  , update
  , maskEvent
  , pin
  )
where

import Control.Lens
import Control.Concurrent.Chan.Unagi
import Control.Monad (void)
import Control.Monad.Trans
import UnliftIO.MVar

import KMonad.Core
import qualified Data.IntSet as S


--------------------------------------------------------------------------------

-- | The EventTracker type allows for requesting future 'EventComparison' data
data EventTracker = EventTracker
  { _inChan   :: InChan KeyEvent  -- ^ A Chan that, when written to, updates all requests
  , _outChan  :: OutChan KeyEvent -- ^ The output of that Chan, held on to to keep empty. NOTE: is this necessary?
  , _masked   :: MVar S.IntSet
  , _curEvent :: MVar KeyEvent    -- ^ An MVar containing the most recent 'KeyEvent'
  }
-- | Define a classy lens for access
makeClassy ''EventTracker

-- | Return a new EventTracker object
mkEventTracker :: MonadIO m => m EventTracker
mkEventTracker = uncurry EventTracker <$> liftIO newChan
                                      <*> newMVar S.empty
                                      <*> newMVar (error "unreachable")

-- | Send an 'EventComparison' to all registered trackers
update :: MonadIO m => KeyEvent -> EventTracker -> m Bool
update ke tr = liftIO $ do
  -- pprint $ "Updating to: " <> pretty ke
  -- Update the internal 'current event'
  _ <- swapMVar (tr^.curEvent) ke

  -- Transmit current-event comparisons to all active `pin`s
  writeChan (tr^.inChan) ke
  void $ readChan  (tr^.outChan) -- Make sure the original outchan gets emptied: NOTE is this necessary?

  -- Return whether this event should be handled
  (not . S.member (fromEnum $ ke^.keyCode)) <$> readMVar (tr^.masked)

-- | Return an action that provides a blocking read for the next event
pin :: MonadIO m => EventTracker -> m (m KeyEvent)
pin tr = do
  oc <- liftIO . dupChan $ tr^.inChan
  pure $ liftIO $ readChan oc

-- | Add the current keycode to the mask and return the action to unmask again
maskEvent :: MonadIO m => EventTracker -> m (m ())
maskEvent tr = liftIO $ do
  kc <- fromEnum . (view keyCode) <$> (readMVar $ tr^.curEvent)
  modifyMVar_ (tr^.masked) $ pure . S.insert kc
  pure $ liftIO . modifyMVar_ (tr^.masked) $ pure . S.delete kc
