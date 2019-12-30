module KMonad.Testing

where

import Prelude

import Data.LayerStack

import KMonad.Button
import KMonad.Daemon
import KMonad.Components.KeyHandler
import KMonad.Keyboard
import KMonad.Keyboard.IO
import KMonad.Keyboard.IO.Linux.UinputSink
import KMonad.Keyboard.IO.Linux.DeviceSource
import KMonad.Util

import qualified RIO.HashMap as M
import qualified RIO.Text    as T

emitB :: Keycode -> ButtonCfg
emitB c = ButtonCfg
  { _pressAction   = Emit $ pressKey   c
  , _releaseAction = Emit $ releaseKey c
  }

kbd :: FilePath
kbd = "/dev/input/by-id/usb-ErgoDox_EZ_ErgoDox_EZ_0-event-kbd"

kmap :: Keymap ButtonCfg
kmap = let ls = mkLayerStack ["test"] $
            [ ("test",
                [ (KeyA, emitB KeyA)
                , (KeyR, emitB KeyS)
                , (KeyS, emitB KeyD)
                , (KeyT, emitB KeyF) ])
            ]
       in case ls of
            Left _   -> error "boop"
            Right it -> it


rstore :: M.HashMap Keycode Char
rstore = M.empty

-- runTest :: IO ()
-- runTest = do
--   logOptions <- logOptionsHandle stdout False
--   withLogFunc logOptions $ \lf -> do
--     let cfg = DaemonCfg
--           { _keySinkA   = uinputSink (defUinputCfg & postInit .~ cmd)
--           , _keySourceA = deviceSource64 kbd
--           , _keymap     = kmap
--           , _logFunc    = lf
--           , _port       = ()
--           }
--     runRIO cfg startDaemon
--   where
--     cmd = Nothing


testKeyIO :: IO ()
testKeyIO = do
  logOptions <- logOptionsHandle stdout True
  withLogFunc logOptions $ \lf -> runRIO (RunEnv RunCfg lf) $ do
    srcR <- deviceSource64 kbd
    snkR <- uinputSink defUinputCfg
    with srcR $ \src -> with snkR $ \snk -> forever $ do
      e <- awaitKeyWith src
      logInfo $ pprintDisp e
      -- traceIO $ pprint e
      emitKeyWith snk (e^.thing)


      -- pure ()
    -- with srcR $ \src -> forever $ do
    --   e <- awaitKeyWith src
    --   liftIO $ print e
    -- with (uinputSink defUinputCfg) $ \snk ->
    --   with srcR $ \src -> forever $ do
    --     e <- awaitKeyWith src
    --     liftIO $ print e
    --     liftIO $ emitKeyWith snk (e^.thing)
    -- cmd = Just. unwords $ [ "/run/wrappers/bin/sudo "
    --                       , "/run/current-system/sw/bin/modprobe"
    --                       , "uinput" ]
