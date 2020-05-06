module KMonad.Daemon.Server
  ( mkServer'
  , mkServer
  , Server
  , HasServer
  , server
  , recvMsg
  , Port
  )
where

import KPrelude

import KMonad.Event
import KMonad.Util

import Network.Socket
import Network.Socket.ByteString

import RIO.List (headMaybe)

--------------------------------------------------------------------------------
-- $typ

-- | The Port used to serve or send messages
type Port = String

-- | Things that can go wrong with the Message server
data ListenerError
  = CouldNotResolvePort Port
  | CouldNotDecodeMessage String
  deriving Show
instance Exception ListenerError

-- | Server environment
data Server = Server
  { _port :: Port
  , _sock :: Socket
  }
makeClassy ''Server

mkServer' :: HasLogFunc e => Port -> (Server -> RIO e a) -> RIO e a
mkServer' p f = bracket (getSock p) (liftIO . close) $ \sck -> f (Server p sck)
  -- do
  -- -- Package the server information
  -- let srvr = Server p sck
  -- -- Start the copying thread
  -- withLaunch_ "message-server" (copyMsg srvr) $ f srvr

mkServer :: HasLogFunc e => Port -> ContT r (RIO e) Server
mkServer = ContT . mkServer'

-- awaitMsg :: (HasLogFunc e, HasServer e) => RIO e Message
-- awaitMsg = view (server.msgV) >>= takeMVar


--------------------------------------------------------------------------------
-- $serv

-- | Open and configure a socket on the listening port
getSock :: HasLogFunc e => Port -> RIO e Socket
getSock p = do
  -- Get address info
  let hints = defaultHints { addrFlags      = [AI_PASSIVE]
                           , addrSocketType = Stream }
  let addrs = liftIO $ getAddrInfo (Just hints) (Just "localhost") (Just p)
  addr <- headMaybe <$> addrs >>= \case
    Nothing -> do
      logError $ "Could not resolve port: " <> fromString (show p)
      throwIO  $ CouldNotResolvePort p
    Just a -> pure a

  -- Open and configure socket
  liftIO $ do
    sck <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption sck ReuseAddr 1
    setSocketOption sck ReusePort 1
    withFdSocket sck $ setCloseOnExecIfNeeded
    bind sck $ addrAddress addr
    listen sck 1
    return sck

-- | Read a message from the socket
recvMsg :: (HasLogFunc e, HasServer e) => RIO e MsgEvent
recvMsg = do
  (conn, _) <- liftIO . accept =<< view sock
  logInfo "Receiving incoming message"
  bytes <- (liftIO $ recv conn 4096)
    `finally` (liftIO $ gracefulClose conn 5000)
  case runGet (get :: Get Message) bytes of
    Left err  -> throwIO $ CouldNotDecodeMessage err
    Right msg -> stampNow msg

-- | Decode the message from the Socket and inject it into KMonad
-- recvMsg :: HasLogFunc e => Socket -> RIO e ()
-- recvMsg sock = do
--   bytes <- liftIO $ recv sock 4096
--   case runGet (get :: Get Message) bytes of
--     Left err -> do
--       logError $ "Could not decode message: " <> fromString err
--       throwIO $ CouldNotDecodeMessage err
--     Right msg -> do
--       logInfo $ "Injecting message: " <> fromString (show msg)
--       inject $ MessageEvent msg

--------------------------------------------------------------------------------
-- $client

getClientSocket :: HasLogFunc e => Port -> RIO e Socket
getClientSocket p = do
  -- Get address info
  let hints = defaultHints { addrSocketType = Stream }
  let addrs = liftIO $ getAddrInfo (Just hints) (Just "localhost") (Just p)
  addr <- headMaybe <$> addrs >>= \case
    Nothing -> do
      logError $ "Could not resolve port: " <> fromString (show p)
      throwIO  $ CouldNotResolvePort p
    Just a -> pure a

  -- Connect to the socket
  liftIO $ do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock $ addrAddress addr
    pure sock


-- | Send a message to the port
sendMsg :: HasLogFunc e => Port -> Message -> RIO e ()
sendMsg p msg = bracket (getClientSocket p) (liftIO . close) go
  where
    go sock = liftIO . sendAll sock $ encode msg
