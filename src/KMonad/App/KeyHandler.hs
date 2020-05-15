module KMonad.Daemon.KeyHandler

where


import KPrelude

import KMonad.Button
import KMonad.Keyboard
import KMonad.Util



-- | The configuration of a 'Button' with some additional state to keep track of
-- the last 'Switch'
data ButtonEnv = ButtonEnv
  { _beButton   :: !Button              -- ^ The configuration for this button
  , _binding    :: !Keycode             -- ^ The 'Keycode' to which this button is bound
  , _lastAction :: !(MVar Switch) -- ^ State to keep track of last manipulation
  }
makeClassy ''ButtonEnv

-- | Anything that has a ButtonEnv has a Button
instance {-# OVERLAPS #-} (HasButtonEnv e) => HasButton e
  where button = buttonEnv . beButton

-- | Initialize a 'Button' from a 'Button' and a binding
mkButtonEnv :: Button -> Keycode -> RIO e ButtonEnv
mkButtonEnv b c = ButtonEnv b c <$> newMVar Release

-- | Run a 'Button' on a 'Switch' returning an 'Action' to be performed by
-- the engine. This will only do something if a 'Press' is followed a 'Release'
-- or vice-versa, pressing or releasing more than once in sequence does nothing.
runButton :: HasButtonEnv e => Switch -> RIO e (Maybe (Action ()))
runButton a = do
  b <- view buttonEnv
  modifyMVar (b^.lastAction) $ \l -> pure $ case (a, l) of
    (Press, Release) -> (Press,   Just $ b^.pressAction)
    (Release, Press) -> (Release, Just $ b^.releaseAction)
    _                -> (a,       Nothing)


--------------------------------------------------------------------------------
-- $env
--

-- |
data KeyHandler = KeyHandler
  { _keymap :: IORef (Keymap ButtonEnv)
  , _baseL  :: IORef LayerTag
  }
makeClassy ''KeyHandler

mkKeyHandler' :: ()
  => LayerTag         -- ^ The initial base layer
  -> Keymap Button    -- ^ The keymap of 'Button's
  -> RIO e KeyHandler
mkKeyHandler' n m = do
  envs <- m & Ls.items . itraversed %%@~ \(_, c) b -> mkButtonEnv b c
  KeyHandler <$> newIORef envs <*> newIORef n

mkKeyHandler :: LayerTag -> Keymap Button -> ContT r (RIO e) KeyHandler
mkKeyHandler n = lift . mkKeyHandler' n

--------------------------------------------------------------------------------
-- $keyh

-- | Lookup the 'ButtonEnv' currently mapped to the key press.
--
-- This returns Just 'ButtonEnv' when the input event is a 'Press' and there is
-- a 'ButtonEnv' stored in the 'keymap' at the corresponding 'Keycode'. In any
-- other scenario this returns 'Nothing'.
lookupKey :: HasKeyHandler e
  => KeyEvent               -- ^ The 'Event' to handle
  -> RIO e (Maybe ButtonEnv) -- ^ The resulting action
lookupKey a
  | isPress a = do
      -- try to read from the keymap
      km <- readIORef =<< view keymap
      case km ^? Ls.atKey (a^.keycode) of
        Nothing -> do
          -- try to read from the base layer
          base <- readIORef =<< view baseL
          pure $ km ^? Ls.inLayer base (a^.keycode)
        e -> pure e
  | otherwise = pure $ Nothing


--------------------------------------------------------------------------------
-- $manip
--
-- We provide 2 actions to manipulate the the layer-stack. One that adds a layer
-- to the front, and one that pops the first occurence of a layer. Note that
-- both of these actions can throw errors.

-- | Print a header message followed by an enumeration of the layer-stack
debugReport :: (HasLogFunc e, HasKeyHandler e) => Utf8Builder -> RIO e ()
debugReport hdr = do
  st <- view Ls.stack <$> (readIORef =<< view keymap)
  let ub = foldMap (\(i, n) -> " "  <> display i
                            <> ". " <> display n <> "\n")
             (zip ([1..] :: [Int]) st)
  ls <- readIORef =<< view baseL
  logDebug $ hdr <> "\n" <> ub <> "Base-layer: " <> display ls <> "\n"

-- | Perform operations on the layer-stack
layerOp :: (HasLogFunc e, HasKeyHandler e) => LayerOp -> RIO e ()
layerOp (PushLayer n) = do
  km <- view keymap
  Ls.pushLayer n <$> readIORef km >>= \case
    Left e   -> throwIO e
    Right m' -> writeIORef km m'
  debugReport $ "Pushed layer to stack: " <> display n

layerOp (PopLayer n) = do
  km <- view keymap
  Ls.popLayer n <$> readIORef km >>= \case
    Left e   -> throwIO e
    Right m' -> writeIORef km m'
  debugReport $ "Popped layer from stack: " <> display n

layerOp (SetBaseLayer n) = do
  (n `elem`) . view Ls.maps <$> (readIORef =<< view (keymap)) >>= \case
    True  -> view baseL >>= \b -> writeIORef b n
    False -> throwIO $ Ls.LayerDoesNotExist n
  debugReport $ "Set base layer to: " <> display n
