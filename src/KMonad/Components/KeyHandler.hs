module KMonad.Components.KeyHandler
  -- ( -- * Things that can go wrong with the 'KeyHandler'
  --   -- $err
  --   KeymapError(..)

  --   -- * The KeyHandler environment
  --   -- $keyh
  -- , KeyHandler
  -- , HasKeyHandler(..)
  -- , mkKeyHandler
  -- , handleKey
  -- , pushLayer
  -- , removeLayer
  -- )
where

import KMonad.Prelude

import KMonad.Button
import KMonad.Keyboard
import KMonad.Util
import RIO.List (delete)

import qualified RIO.HashMap as M
import qualified RIO.Set     as S

--------------------------------------------------------------------------------
-- $err
data KeymapError
  = LayerNotFoundError   Name
  | LayerNotOnStackError Name
  deriving Show

instance Exception KeymapError

--------------------------------------------------------------------------------
-- $keymap
--
-- The 'Keymap' part of the 'KeyHandler' is a pure implementation of a stack of
-- 'Layer's. 'Keymap's can only be created, there is no support for changing the
-- layers on the fly. The changes that are supported is adding layers to the
-- stack and deleting them from the stack again.
--
-- Items are looked up in a 'Keymap' by checking the map at the front of the
-- stack first, if it succeeds, that is the value that is returned. If that
-- fails, the next map in the stack is tried, etc.

-- | A 'Layer' is a mapping from 'Keycode's to some object
newtype Layer a = Layer { unLayer :: M.HashMap Keycode a}
  deriving (Show, Eq, Ord, Functor)

data Keymap a = Keymap
  { _stack :: ![Name]                        -- ^ The current stack of layers
  , _maps  :: !(S.Set Name)                  -- ^ A set of all 'Layer' names
  , _items :: !(M.HashMap (Name, Keycode) a) -- ^ The map of all the bindings
  } deriving (Show, Eq, Functor)
makeLenses ''Keymap

-- | Create a new 'Keymap' from a HashMap of Names to Layers
--
-- We store all items in a single 'HashMap' under the (Name, Keycode) tuple,
-- this way we only have to look up once per layer in the stack (instead of:
-- lookup layer -> lookup key)
mkKeymap :: M.HashMap Name (Layer a) -> Keymap a
mkKeymap ls = Keymap
  { _stack = []
  , _maps  = S.fromList . M.keys $ ls
  , _items = M.fromList $ ls ^@.. ifolded <.> (to unLayer . ifolded)
  }

-- | Return a fold of all the items currently mapped to the 'Keycode'
--
-- This can be used with 'toListOf' to get an overview of all the items
-- currently mapped to a 'Keycode', or more usefully, with 'firstOf' to simply
-- try a lookup like this: `keymap ^? atKey KeyA`
atKey :: Keycode -> Fold (Keymap a) a
atKey c = folding $ \m -> m ^.. stack . folded . to (getK m) . folded
  where getK m n = fromMaybe [] (pure <$> M.lookup (n, c) (m^.items))

-- | Add a layer to the front of the stack and return the new 'Keymap'. If the
-- 'Layer' does not exist, return a 'KeymapError'
pushLayer :: Name -> Keymap a -> Either KeymapError (Keymap a)
pushLayer name keymap = if name `elem` keymap^.stack
  then Right $ keymap & stack %~ (name:)
  else Left  $ LayerNotFoundError name

-- | Remove a layer from the stack. If the 'Name' does not exist on the stack,
-- return a 'LayerNotOnStackError', if the 'Name' does not exist at all in the
-- 'Keymap', return a 'LayerNotFoundError'.
popLayer :: Name -> Keymap a -> Either KeymapError (Keymap a)
popLayer name keymap = if
  | name `elem` keymap^.stack -> Right $ keymap & stack %~ delete name
  | name `elem` keymap^.maps  -> Left  $ LayerNotOnStackError name
  | True                      -> Left  $ LayerNotFoundError   name


--------------------------------------------------------------------------------
-- $keyh
--
-- The 'KeyHandler' is the stateful component responsible for maintaining the
-- state of the 'Keymap' and dispatching incoming 'KeyAction's to the right
-- 'Button's. The 'KeyHandler' handles an event in the following way:
--
-- 1. First it checks for a 'PriorityButton' and uses that if it finds one
--    (removing it from the priority-store in the process).
-- 2. Otherwise, if it is a 'Press', it checks the current 'Keymap'. If it finds
-- one then it presses that button and puts it in the release-store.
-- 3. Otherwise, if it is a 'Release', it checks the release-store and uses the
-- 'Button' from there (removing it from the store in the process).
--
-- This way 'Action's can short-circuit the handling of a particular 'KeyAction'
-- by registering 'PriorityButton's. Additionally, it ensures that a 'Release'
-- will always be handled by the 'Button' that initiated the 'Press'. This
-- prevents issues with pressing a button, then changing layers, and then no
-- longer being able to release that button, because its location in the stack
-- has been overwritten.

-- | The stateful aspect of a 'KeyHandler'. This will all be stored in 1 'MVar'.
data KhST = KhST
  { _keymap        :: !(Keymap Button)
    -- ^ The current mapping of keycodes to buttons
  , _releaseStore  :: !(M.HashMap Keycode Button)
    -- ^ The store of buttons to use to handle releases
  , _priorityStore :: !(M.HashMap KeyAction PriorityButton)
    -- ^ The store of buttons to check before anything else
  }
makeLenses ''KhST

-- handled :: KeyAction -> Traversal' KhST Action
handled :: KeyAction -> Maybe (Action, KhST)
handled ka st
  | ka `M.member` st^.priorityStore ->

a = undefined


-- handled :: KeyAction -> Fold KhST Action
-- handled a = folding $ \st ->
--   let v = st^..priorityStore.at a.folded.action
--        <> st^..
--   in v

-- | All the environment and state required for 'KeyHandler' operations.
data KeyHandler e = KeyHandler
  { _buttonEnv :: Button -> ButtonEnv e
  , _khST      :: !(MVar KhST)
  }
makeLenses ''KeyHandler

class HasKeyHandler e where
  keyHandler :: Lens' e (KeyHandler e)

instance HasMST (KeyHandler e) KhST where
  getMST = khST

-- | Create a 'KeyHandler' object from a 'Keymap'
mkKeyHandler :: MonadIO m
  => (Button -> ButtonEnv e) -- ^ The function used to generate 'ButtonEnv's
  -> Keymap Button           -- ^ The keymap containing action bindings
  -> m (KeyHandler e)        -- ^ The action that creates a 'KeyHandler'
mkKeyHandler be km = KeyHandler be <$> newMVar (KhST km M.empty M.empty)


-- | Handle a 'KeyAction' using the 'KeyHandler'
handleKey :: HasKeyHandler e => KeyAction -> RIO e ()
handleKey a = do undefined
  -- b <- withMST $ \st -> undefined -- if
  --   -- | a `elem` st^.priorityStore -> undefined
  -- undefined


--   -- See if 'a' matches any priority handler

--   priV <- view priorityHandlers
--   runP <- modifyMVar priV $ \pri -> do
--             case M.lookup (a^.keyAction) pri of
--               Nothing -> pure (pri, Nothing)
--               Just p  -> pure (M.delete (a^.keyAction) pri, Just p)
--   case runP of
--     Nothing -> nonPriority
--     Just p  -> runPriorityButton p

--   where
--     nonPriority
--       -- Handle presses by looking them up in the keyMap
--       | isPress a = do
--           km <- readMVar =<< (view keyMap)
--           case S.lookup (a^.keycode) km of
--             Nothing -> pure ()
--             Just b  -> do
--               rs <- view releaseStore
--               modifyMVar_ rs $ pure . M.insert (a^.keycode) b
--               local (& button .~ b) $ pressButton

--       -- Handle releases by looking them up in the releaseStore
--       | otherwise = do
--           rsV <- view releaseStore
--           rs  <- takeMVar rsV
--           case M.lookup (a^.keycode) rs of
--             Nothing -> putMVar rsV rs
--             Just b  -> do
--               putMVar rsV $ M.delete (a^.keycode) rs
--               local (& button .~ b) $ releaseButton

-- -- -- -- | Push a LayerId to the front of the 'Keymap'. This throws an error if the
-- -- -- -- provided 'Name' does not correspond to any map.
-- -- -- pushLayer :: (HasKeyHandler e, HasLogFunc e) => Name -> RIO e ()
-- -- -- pushLayer n = view keyMap >>= flip modifyMVar_ go
-- -- --   where
-- -- --     go km = case S.push n km of
-- -- --       Nothing -> do
-- -- --         logError $ "Tried to push non-existant layer: " <> fromString (show n)
-- -- --         throwIO $ LayerNotFoundError n
-- -- --       Just km' -> do
-- -- --         logInfo $ "Pushing layer to stack: " <> fromString (show n)
-- -- --         pure km'

-- -- -- -- | Remove a LayerId from the 'Keymap'. This throws an error is the provided
-- -- -- -- 'Name' does not currently exist in the layer stack.
-- -- -- removeLayer :: (HasKeyHandler e, HasLogFunc e) => Name -> RIO e ()
-- -- -- removeLayer n = view keyMap >>= flip modifyMVar_ go
-- -- --   where
-- -- --     go km = case S.pop n km of
-- -- --       Nothing -> do
-- -- --         logError $ "Tried to delete non-existant layer: " <> fromString (show n)
-- -- --         throwIO $ LayerNotFoundError n
-- -- --       Just km' -> do
-- -- --         logInfo $ "Deleting layer from stack: " <> fromString (show n)
-- -- --         pure km'



-- -- -- -- --------------------------------------------------------------------------------
-- -- -- -- -- Constructing a LayerStack from a set of tokens

-- -- -- -- -- | Turn a nested set of tokens into a layer stack of operations
-- -- -- -- mkLayerStack :: (CanButton m, MonadIO n)
-- -- -- --   => [(Name, [(KeyCode, ButtonToken)])]
-- -- -- --   -> Name
-- -- -- --   -> n (LayerStack m)
-- -- -- -- mkLayerStack ts def = do
-- -- -- --   -- There is probably a much prettier lensy way of doing this
-- -- -- --   bs <- mapM (mapTup (mapM (mapTupB encode))) ts
-- -- -- --   h  <- newMVar . myFromJust "making layer error" . push def . mkMapStack $ bs
-- -- -- --   LayerStack h <$> newMVar (M.empty)
-- -- -- --   where
-- -- -- --     mapTup  f (c, a) = (c,) <$> f a
-- -- -- --     mapTupB f (c, a) = (c,) <$> f c a
