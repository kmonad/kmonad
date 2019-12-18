module KMonad.Domain.KeyMap
  -- ()
where

import KMonad.Prelude

import KMonad.Core


--------------------------------------------------------------------------------

-- | A LayerStack object that manages overlapping sets of handlers
data LayerStack m = LayerStack
  { mapStack   :: MVar (S.MapStack Name KeyCode (Button m))
  , mapRelease :: MVar (M.HashMap KeyCode (Button m))}

-- | A ClassyLens style typeclass to describe 'having a LayerStack'
class MonadIO m => HasLayerStack r m where
  layerStack :: Lens' r (LayerStack m)

-- | Use a LayerStack to handle a single key event
handleWith :: (MonadButton m, MonadIO m) => KeyEvent -> LayerStack m -> m ()
handleWith e ls
  -- Handle presses by looking up the key in the layerstack
  | isPress e = do
      let kc = e^.keyCode
      ms  <- readMVar . mapStack $ ls
      case S.lookup (e^.keyCode) ms of
        Nothing -> return ()
        Just b  -> do
          liftIO . modifyMVar_ (mapRelease ls) $ return . M.insert kc b
          press b

  -- Handle releases by finding the key that initiated the press
  | isRelease e = do
      let kc = e^.keyCode
      rels <- takeMVar (mapRelease ls)
      case M.lookup kc rels of
        Nothing -> putMVar (mapRelease ls) rels
        Just b  -> do
          putMVar (mapRelease ls) (M.delete kc rels)
          release b

  -- This should be unreachable
  | otherwise = pure ()


myFromJust :: String -> Maybe a -> a
myFromJust = fromMaybe . error

-- | Push a LayerID to the top of the stack NOTE: the monads do not necessarily
-- have to line up, maybe change this? FIXME: Add error handling and remove the
-- FromJust (or not... I can ensure there are no references to nonexistent
-- layers when I compile the Config)
pushLS :: MonadIO m => Name -> LayerStack m -> m ()
pushLS lid ls = liftIO $ modifyMVar_ (mapStack ls) $ return . myFromJust ("Cannot find layer: " <> T.unpack lid) . push lid

-- | Pop a Name from the stack
popLS :: MonadIO m => Name -> LayerStack m -> m ()
popLS lid ls = liftIO $ modifyMVar_ (mapStack ls) $ return . pop lid


--------------------------------------------------------------------------------
-- Constructing a LayerStack from a set of tokens

-- | Turn a nested set of tokens into a layer stack of operations
mkLayerStack :: (CanButton m, MonadIO n)
  => [(Name, [(KeyCode, ButtonToken)])]
  -> Name
  -> n (LayerStack m)
mkLayerStack ts def = do
  -- There is probably a much prettier lensy way of doing this
  bs <- mapM (mapTup (mapM (mapTupB encode))) ts
  h  <- newMVar . myFromJust "making layer error" . push def . mkMapStack $ bs
  LayerStack h <$> newMVar (M.empty)
  where
    mapTup  f (c, a) = (c,) <$> f a
    mapTupB f (c, a) = (c,) <$> f c a
