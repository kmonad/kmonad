{-|
Module      : KMonad.App.BEnv
Description : Implementation details behind 'Button'
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

When running KMonad, we need to keep track the last switchstate of a 'Button',
because we only allowing switching, (we have to filter out repeated 'Press' or
'Release' events). Additionally, we keep track of what 'Keycode' a button is
bound to, to provide the 'myBinding' functionality from 'MonadK'.

-}

module KMonad.App.BEnv
  ( BEnv(..)
  , HasBEnv(..)
  , initBEnv
  , runBEnv
  )
where

import KMonad.Prelude

import KMonad.Action
import KMonad.Button
import KMonad.Keyboard

--------------------------------------------------------------------------------
-- $benv
--
-- When running KMonad, a button also keeps track of what keycode it's bound to,
-- and what its last switch was. This is used to provide the 'myBinding' feature
-- of MonadK, and the invariant that switches always alternate (there is no
-- press-press or release-release).

-- | The configuration of a 'Button' with some additional state to keep track of
-- the last 'Switch'
data BEnv = BEnv
  { _beButton   :: !Button        -- ^ The configuration for this button
  , _binding    :: !Keycode       -- ^ The 'Keycode' to which this button is bound
  , _lastSwitch :: !(MVar Switch) -- ^ State to keep track of last manipulation
  }
makeClassy ''BEnv

instance HasButton BEnv where button = beButton

-- | Initialize a 'BEnv', note that a key is always initialized in an unpressed
-- state.
initBEnv :: MonadIO m => Button -> Keycode -> m BEnv
initBEnv b c = BEnv b c <$> newMVar Release

-- | Try to switch a 'BEnv'. This only does something if the 'Switch' is
-- different from the 'lastSwitch' field. I.e. pressing a pressed button or
-- releasing a released button does nothing.
runBEnv :: MonadUnliftIO m => BEnv -> Switch -> m (Maybe Action)
runBEnv b a =
  modifyMVar (b^.lastSwitch) $ \l -> pure $ case (a, l) of
    (Press, Release) -> (Press,   Just $ b^.pressAction)
    (Release, Press) -> (Release, Just $ b^.releaseAction)
    _                -> (a,       Nothing)
