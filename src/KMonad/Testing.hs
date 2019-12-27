module KMonad.Testing

where

import KMonad.Prelude

import KMonad.Button
import KMonad.Daemon
import KMonad.Components.KeyHandler
import KMonad.Keyboard
import KMonad.Keyboard.IO.Linux.UinputSink
import KMonad.Keyboard.IO.Linux.DeviceSource

import qualified RIO.HashMap as M

emitB :: Keycode -> ButtonCfg
emitB c = ButtonCfg
  { _pressAction   = Emit $ pressKey   c
  , _releaseAction = Emit $ releaseKey c
  }

kbd :: FilePath
kbd = "/dev/input/by-id/usb-ErgoDox_EZ_ErgoDox_EZ_0-event-kbd"

kmap :: Keymap ButtonCfg
kmap = mkKeymap "test" $ M.fromList
  [ ("test", mkLayer $ M.fromList
      [ (KeyA, emitB KeyA)
      , (KeyR, emitB KeyS)
      , (KeyS, emitB KeyD)
      , (KeyT, emitB KeyF) ])
  ]

rstore :: M.HashMap Keycode Char
rstore = M.empty

runTest :: IO ()
runTest = do
  logOptions <- logOptionsHandle stdout False
  withLogFunc logOptions $ \lf -> do
    let cfg = DaemonCfg
          { _keySinkA   = uinputSink (defUinputCfg & postInit .~ cmd)
          , _keySourceA = deviceSource64 kbd
          , _keymap     = kmap
          , _logFunc    = lf
          , _port       = ()
          }
    runRIO cfg startDaemon

  where
    cmd = Nothing
    -- cmd = Just. unwords $ [ "/run/wrappers/bin/sudo "
    --                       , "/run/current-system/sw/bin/modprobe"
    --                       , "uinput" ]
