module KMonad.Testing

where

import KPrelude

import Data.LayerStack

import KMonad.Action
import KMonad.Button
import KMonad.Daemon
import KMonad.Keyboard
import KMonad.Keyboard.IO
import KMonad.Keyboard.IO.Linux.UinputSink
import KMonad.Keyboard.IO.Linux.DeviceSource
import KMonad.Runner
import KMonad.Util

import qualified RIO.HashMap as M

kbd :: FilePath
kbd = "/dev/input/by-id/usb-04d9_daskeyboard-event-kbd"
-- kbd = "/dev/input/by-id/usb-ErgoDox_EZ_ErgoDox_EZ_0-event-kbd"

silly :: Button
silly = mkButton
  ( pause 500 >> emit (keyPress KeyA) >> pause 500 >> emit (keyPress KeyB) )
  ( emit (keyRelease KeyA) >> pause 500 >> emit (keyRelease KeyB) )


kmap :: Keymap Button
kmap = let sftB = modded KeyLeftShift . emitB
           th   = tapHold 500  (emitB KeyZ) (emitB KeyLeftShift)
           tn   = tapNext (emitB KeyZ) (emitB KeyLeftShift)
           mt   = multiTap (emitB KeyC) [(500, emitB KeyA), (500, emitB KeyB)]
           cs   = around (emitB KeyLeftShift) (emitB KeyLeftCtrl)
           ls = mkLayerStack ["test"] $
            [ ("test",
                [ (KeyA, emitB KeyA)
                , (KeyS, emitB KeyR)
                , (KeyD, emitB KeyS)
                , (KeyF, emitB KeyT)
                , (KeyQ, sftB KeyA)
                , (KeyW, sftB KeyR)
                , (KeyE, sftB KeyS)
                , (KeyR, sftB KeyT)
                , (KeyZ, th)
                , (KeyX, tn)
                , (KeyC, mt)
                , (KeyV, cs)
                ])
            ]
       in case ls of
            Left _   -> error "boop"
            Right it -> it


rstore :: M.HashMap Keycode Char
rstore = M.empty

runTest' :: LogLevel -> IO ()
runTest' ll = run (defRunCfg & logLevel .~ ll) $ do

  snkDev <- uinputSink defUinputCfg
  srcDev <- deviceSource64 kbd

  let dcfg = DaemonCfg
        { _keySinkDev   = snkDev
        , _keySourceDev = srcDev
        , _keymapCfg    = kmap
        , _port         = ()
        }
  runDaemon dcfg loop

launchTest :: IO ()
launchTest = run (defRunCfg & logLevel .~ LevelInfo) $ do
  flip (withLaunch_ "testing") (threadDelay 200000) $ do
      threadDelay 10000
      throwString "hello"

runTest :: IO ()
runTest = runTest' LevelDebug

testCfg :: RunCfg
testCfg = defRunCfg & logLevel .~ LevelInfo

-- testKeyIO :: LogLevel -> IO ()
-- testKeyIO ll = run (defRunCfg & logLevel .~ ll) $ do
--   srcR <- deviceSource64 kbd
--   snkR <- uinputSink defUinputCfg
--   with srcR $ \src -> with snkR $ \snk -> forever $ do
--     e <- awaitKeyWith src
--     logInfo $ displayShow e
--     emitKeyWith snk (e^.thing)
