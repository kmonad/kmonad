{-
This code defines the daemon-server functionality of KMonad, that allows KMonad
to run in the background yet receive signals from the CLI.

-}
module KMonad.Api.Server

where

import Control.Concurrent (forkFinally)
import Control.Monad
import Data.Serialize
import Network.Socket
import Network.Socket.ByteString
import UnliftIO

import KMonad.Core
import KMonad.Domain.Effect

import qualified Data.Text as T

-- | The server-loop that awaits connections on the provided port and runs the
-- handler on any request.
serve :: (MonadIO m, MonadInject m, MonadLogger m)
  => ServiceName -> m a
serve port = liftIO . withSocketsDo $ do
  addr <- resolve
  bracket (open addr) close loop

  where
    resolve = do
      let hints = defaultHints { addrFlags      = [AI_PASSIVE]
                               , addrSocketType = Stream }
      head <$> getAddrInfo (Just hints) (Just "localhost") (Just port)

    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      setSocketOption sock ReusePort 1
      withFdSocket sock $ setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1
      return sock

    loop sock = forever $ do
      (conn, _peer) <- accept sock
      -- NOTE: CONTINUE HERE, after error refactor
      void $ forkFinally (handleConn conn) (const $ gracefulClose conn 5000)

-- | Handle 1 request made by the client
handleConn :: (MonadIO m, MonadInject m, MonadLogger m) => Socket -> m ()
handleConn sock = do
  bytes <- liftIO $ recv sock 4096
  case runGet (get :: Get Event) bytes of
    Left err -> $(logError) (T.pack err)
    Right ev -> injectEvent ev

-- | Send 1 request to the server
