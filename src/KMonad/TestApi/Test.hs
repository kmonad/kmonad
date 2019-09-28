module KMonad.Api.Test
  (
  )
where

import Control.Monad.Reader
import qualified Data.Text as T
import UnliftIO

import KMonad.Core
import KMonad.Core.Parser
import KMonad.Core.Pretty
import KMonad.Domain.Loop
import KMonad.Domain.Effect.Handler
import KMonad.Domain.Effect.KeyIO
import KMonad.Domain.Effect.Trace


data TestOut = TestOut
  { keysOut   :: [KeyEvent]
  , tracesOut :: [Text]
  }
  deriving (Eq, Show)

instance Pretty TestOut where
  pretty o = "------ Registered key events ------\n"
          <> T.unlines (map pretty (keysOut o))
          <> "------ Registered traces ----------\n"
          <> T.unlines (tracesOut o)

data TestApp = TestApp
  { inputs  :: IORef [Event]
  , outputs :: MVar  [KeyEvent]
  , traces  :: MVar  [Text]
  }

instance MonadNext (ReaderT TestApp IO) where
  nextEvent = do
    h <- asks inputs
    atomicModifyIORef' h $ \case []     -> ([], Quit)
                                 (e:es) -> (es, e)

instance MonadEmit (ReaderT TestApp IO) where
  emitKey e = do
    h <- asks outputs
    modifyMVar_ h (return . ([e]<>))

instance MonadTrace (ReaderT TestApp IO) where
  trace t = do
    h <- asks traces
    modifyMVar_ h (return . ([t]<>))

instance MonadHandler (ReaderT TestApp IO) where
  handle = emitKey

testSequence :: [KeyEvent]
testSequence = concatMap f $ parseE (some keycodeP) "h e l l o"
  where f c = [ KeyEvent Press   c $ mkTime 0 0
              , KeyEvent Release c $ mkTime 0 0 ]

loopTest :: [KeyEvent] -> IO TestOut
loopTest es = do
  app <- TestApp <$> newIORef (map InputEvent es) <*> newMVar [] <*> newMVar []
  runReaderT loop app
  TestOut <$> (reverse <$> readMVar (outputs app))
          <*> (reverse <$> readMVar (traces  app))
