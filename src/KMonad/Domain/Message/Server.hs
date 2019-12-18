{-|
Module      : KMonad.Domain.Message.Server
Description : The part of KMonad that listens for incoming messages
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

KMonad is structured with a basic daemon/client architecture, where running
KMonad forks a background process. The user can interact with this process by
sending messages to a port on localhost. This is the code that is responsible
for listening to incoming messages.
-}
module KMonad.Domain.Message.Server
  ( startMessageServer
  )
where

import KMonad.Prelude

import Data.Serialize
import Network.Socket
import Network.Socket.ByteString
import UnliftIO.Concurrent

import KMonad.Domain.Effect
import KMonad.Domain.Event
import KMonad.Domain.Message.Message

import qualified RIO.List as L


--------------------------------------------------------------------------------

-- | Things that can go wrong with the Message server
data ListenerError
  = CouldNotResolvePort ServiceName
  | CouldNotDecodeMessage String
  deriving Show
instance Exception ListenerError


--------------------------------------------------------------------------------

-- | The functionality needed by the message server
type CanListen e = (HasLogFunc e, HasInjectFunc e)

-- | Start the message server on the provided port
startMessageServer :: CanListen e => ServiceName -> RIO e ()
startMessageServer p = bracket (getSock p) (liftIO . close) serve

-- | Open and configure a socket on the listening port
getSock :: HasLogFunc e => ServiceName -> RIO e Socket
getSock p = do
  -- Get address info
  let hints = defaultHints { addrFlags      = [AI_PASSIVE]
                            , addrSocketType = Stream }
  let addrs = liftIO $ getAddrInfo (Just hints) (Just "localhost") (Just p)
  addr <- L.headMaybe <$> addrs >>= \case
    Nothing -> do
      logError $ "Could not resolve port: " <> fromString (show p)
      throwIO  $ CouldNotResolvePort p
    Just a -> pure a

  -- Open and configure socket
  liftIO $ do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption sock ReuseAddr 1
    setSocketOption sock ReusePort 1
    withFdSocket sock $ setCloseOnExecIfNeeded
    bind sock $ addrAddress addr
    listen sock 1
    return sock

-- | Whenever a message is sent, fork of a message handler
serve :: CanListen e => Socket -> RIO e ()
serve sock = do
  logInfo "Starting to listen for messages"
  forever $ do
    (conn, _) <- liftIO $ accept sock
    forkFinally (recvMsg conn) (const . liftIO $ gracefulClose conn 5000)

-- | Decode the next inject it into KMonad when it arrives
recvMsg :: CanListen e => Socket -> RIO e ()
recvMsg sock = do
  bytes <- liftIO $ recv sock 4096
  case runGet (get :: Get Message) bytes of
    Left err -> do
      logError $ "Could not decode message: " <> fromString err
      throwIO $ CouldNotDecodeMessage err
    Right msg -> do
      logInfo $ "Injecting message: " <> fromString (show msg)
      inject $ MessageEvent msg
