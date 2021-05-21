
module KMonad.Pullchain.Operations

where


import KMonad.Prelude

import KMonad.Model
import KMonad.Pullchain.Button
import KMonad.Pullchain.Types

import qualified RIO.HashMap as M

-- | Compile an entire 'Keymap' of 'BCfg's into runable 'Button's
initKeymap :: Keymap BCfg -> Keymap Button
initKeymap = map (second $ M.map b)

--------------------------------------------------------------------------------
-- $button

-- | How to turn a 'BCfg' into a runable action button
mkButton :: BCfg -> Button
mkButton = b

b :: BCfg -> Button
-- General
b (BEmit c) = emitB c
b  BBlock    = pass
b (BPause ms) = doPause ms

-- Layer manip
b (BLayerToggle n) = layerToggle n
b (BLayerSwitch n) = layerSwitch n
b (BLayerAdd n) = layerAdd n
b (BLayerRem n) = layerRem n
b (BLayerDelay ms n) = layerDelay ms n
b (BLayerNext n) = layerNext n

-- Tap-hold functionality
b (BTapNext t h) = tapNext (b t) (b h)
b (BTapHold ms t h) = tapHold ms (b t) (b h)
b (BTapHoldNext ms t h) = tapHoldNext ms (b t) (b h)
b (BTapNextRelease t h) = tapNextRelease (b t) (b h)
b (BTapHoldNextRelease ms t h) = tapHoldNextRelease ms (b t) (b h)

-- Wrapping
b (BAround p q) = around (b p) (b q)
b (BAroundNext p) = aroundNext (b p)
b (BAroundNextSingle p) = aroundNextSingle (b p)

-- Macros
b (BTapMacro ps) = tapMacro $ map b ps
b (BTapMacroRelease ps) = tapMacroRelease $ map b ps
-- b (BComposeSeq ps) = composeSeq $ map b ps

-- Other
b (BMultiTap ps q) = multiTap (b q) (map (second b) ps)
b (BCommand t mt) = cmdButton t mt
b (BStickyKey ms p) = stickyKey ms (b p)



-- pressKey :: IO m => Keycode -> m ()

-- | Trigger the button-action press currently registered to 'Keycode'
--
-- This is the actual dispatch function used to trigger the model to do things.
-- pressKey :: CanK e => Keycode -> RIO e ()
-- pressKey c =
--   view keymap >>= flip Km.lookupKey c >>= \case

--     -- If the keycode does not occur in our keymap
--     Nothing -> do
--       ft <- view fallThrough
--       if ft
--         then do
--           emit $ mkPress c
--           await (isReleaseOf c) $ \_ -> do
--             emit $ mkRelease c
--             pure Catch
--         else pure ()

--     -- If the keycode does occur in our keymap
--     Just b  -> runBEnv b Press >>= \case
--       Nothing -> pure ()  -- If the previous action on this key was *not* a release
--       Just a  -> do
--         -- Execute the press and register the release
--         app <- view appEnv
--         runRIO (KEnv app b) $ do
--           runAction a
--           awaitMy Release $ do
--             runBEnv b Release >>= maybe (pure ()) runAction
--             pure Catch

--------------------------------------------------------------------------------
  -- dsp <- Dp.mkDispatch $ liftIO src
  -- ihk <- Hs.mkHooks    $ Dp.pull  dsp
  -- slc <- Sl.mkSluice   $ Hs.pull  ihk
  -- phl <- Km.mkKeymap (cfg^.firstLayer) (cfg^.keymapCfg)

-- loop :: RIO AppEnv ()
-- loop = forever $ view sluice >>= Sl.pull >>= \case
--   e | e^.switch == Press -> pressKey $ e^.code
--   _                      -> pure ()

  -- Initialize output components
  -- ohk <- Hs.mkHooks . atomically . takeTMVar $ otv
