module KMonad.Domain.Button
  ( runButton
  , pressButton
  , releaseButton
  )
where

import KMonad.Prelude

import UnliftIO.Async      (race)
import UnliftIO.Concurrent (forkIO)

import KMonad.Core
-- import KMonad.Domain.Action
import KMonad.Domain.KeyIO
import KMonad.Domain.Types

type CanButton e =
  ( HasKeySink e
  , HasLogFunc e
  )

-- | Perform a 'SwitchAction' in a 'ButtonEnv'
--
-- If the last action was a 'Release' and this action is a 'Press', then run the
-- 'pressAction'. If the last action was a 'Press' and this action is a
-- 'Release', then run the 'releaseAction', else run no action.
runButton :: CanButton e => SwitchAction -> Button -> RIO e ()
runButton current b = do
  last <- takeMVar $ b^.lastAction
  case (current, last) of
    (Press, Release) -> runAction $ b^.pressAction   -- Press after release
    (Release, Press) -> runAction $ b^.releaseAction -- Release after press
    _                -> pure ()                      -- P&P or R&R
  putMVar (b^.lastAction) current

-- | Send a 'Press' signal to the 'Button'
pressButton :: CanButton e => Button -> RIO e ()
pressButton = runButton Press

-- | Send a 'Release' signal to the 'Button'
releaseButton :: CanButton e => Button -> RIO e ()
releaseButton = runButton Release


-- | Handle all the different 'Action's
runAction :: CanButton e => Action -> RIO e ()
runAction (Emit a)   = emitKey a
runAction (Fork a)   = void . forkIO $ runAction a
runAction (Macro as) = traverse_ runAction as
runAction (Race (a1, a2) (b1, b2)) = do
  race (runAction a1) (runAction b1) >>= \case
    Left  _ -> runAction a2
    Right _ -> runAction b2
runAction (Pause ms) = threadDelay $ fromIntegral ms
runAction Pass       = pure ()
