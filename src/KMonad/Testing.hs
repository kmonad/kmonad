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
import KMonad.Runner
import KMonad.Util

import qualified RIO.HashMap as M


kbd :: FilePath
kbd = "/dev/input/by-id/usb-ErgoDox_EZ_ErgoDox_EZ_0-event-kbd"

kmap :: Keymap Button
kmap = let sftB = modded KeyLeftShift . emitB
           ls = mkLayerStack ["test"] $
            [ ("test",
                [ (KeyA, emitB KeyA)
                , (KeyR, emitB KeyS)
                , (KeyS, emitB KeyD)
                , (KeyT, emitB KeyF)
                , (KeyQ, sftB KeyA)
                , (KeyW, sftB KeyS)
                , (KeyF, sftB KeyD)
                , (KeyP, sftB KeyF)
                ])
            ]
       in case ls of
            Left _   -> error "boop"
            Right it -> it


rstore :: M.HashMap Keycode Char
rstore = M.empty

runTest :: LogLevel -> IO ()
runTest ll = run (defRunCfg & logLevel .~ ll) $ do

  snkDev <- uinputSink defUinputCfg
  srcDev <- deviceSource64 kbd

  let dcfg = DaemonCfg
        { _keySinkDev   = snkDev
        , _keySourceDev = srcDev
        , _keymap       = kmap
        , _port         = ()
        }
  runDaemon dcfg $ startDaemon


testKeyIO :: IO ()
testKeyIO = run defRunCfg $ do
  srcR <- deviceSource64 kbd
  snkR <- uinputSink defUinputCfg
  with srcR $ \src -> with snkR $ \snk -> forever $ do
    e <- awaitKeyWith src
    logInfo $ pprintDisp e
    emitKeyWith snk (e^.thing)
