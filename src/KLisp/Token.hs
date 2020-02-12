module KLisp.Token where

import KPrelude


import KMonad.Button
import KMonad.Keyboard
import KMonad.Keyboard.IO.Linux.UinputSink

data KToken
  = KAtom Text
  | KList [KToken]
  | KText Text
  | KNum  Int
  deriving Show


data IToken
  = KDeviceSource FilePath

data OToken
  = KUinputSink UinputCfg

data IOCfg = IOCfg
  { kInputCfg  :: IToken
  , kOutputCfg :: OToken
  }

-- data KVal
--   = KNum     Int
--   | KStr     Text
--   | KAtom    Text
--   | KButton  Button
--   | KKeycode Keycode
--   | KList    [KVal]
--   | KDeref   Text
--   | KSrcMap  [KVal]
--   | KLayer   [KVal]
--   | KIOCfg   IOCfg
