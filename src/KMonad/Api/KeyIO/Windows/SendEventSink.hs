module KMonad.Api.KeyIO.Windows.SendEventSink

where

import Control.Lens
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import UnliftIO

import KMonad.Core
import KMonad.Api.KeyIO.Types
import KMonad.Api.KeyIO.Windows.Types


--------------------------------------------------------------------------------

foreign import ccall "sendKey"
  sendKey :: Ptr WinKeyEvent -> IO ()


data SKSink = SKSink
  { _buffer :: Ptr WinKeyEvent }
makeClassy ''SKSink

sendEventKeySink :: KeySink
sendEventKeySink = BracketIO
  { _open  = skOpen
  , _close = skClose
  , _use   = skSend
  }

skOpen :: IO SKSink
skOpen = SKSink <$> mallocBytes (sizeOf (undefined :: WinKeyEvent))

skClose :: SKSink -> IO ()
skClose = free . view buffer

skSend :: SKSink -> KeyEvent -> IO ()
skSend snk e = do
  poke (snk^.buffer) (e^.re _KeyEvent)
  sendKey (snk^.buffer)
