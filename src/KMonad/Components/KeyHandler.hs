module KMonad.Components.KeyHandler
  ( -- * Things that can go wrong with Keymap operations
    -- $err
    KeymapError(..)

    -- * The pure implementation of 'Keymap' and 'Layer'
    -- $keymap
  , Layer
  , Keymap
  , mkLayer
  , mkKeymap

    -- * The KeyHandler environment
    -- $keyh
  , KeyHandler
  , HasKeyHandler
  , keyHandler
  , mkKeyHandler
  , handleKey

    -- * Stack manipulation
    -- $manip
  , pushLayer
  , popLayer
  )
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

-- | Create a 'Layer' from an mapping between 'Keycode's and things
mkLayer :: M.HashMap Keycode a -> Layer a
mkLayer = Layer

-- | The 'Keymap' type describes the different mappings between 'Keycode's and
-- 'Button's, and provides functionality for stacking and overlapping those
-- maps.
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
-- 'Layer' does not exist, return a 'KeymapError'.
pushLayer' :: Name -> Keymap a -> Either KeymapError (Keymap a)
pushLayer' name keymap = if name `elem` keymap^.stack
  then Right $ keymap & stack %~ (name:)
  else Left  $ LayerNotFoundError name

-- | Remove a layer from the stack. If the 'Name' does not exist on the stack,
-- return a 'LayerNotOnStackError', if the 'Name' does not exist at all in the
-- 'Keymap', return a 'LayerNotFoundError'.
popLayer' :: Name -> Keymap a -> Either KeymapError (Keymap a)
popLayer' name keymap = if
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
--    one then it presses that button and puts it in the release-store.
-- 3. Otherwise, if it is a 'Release', it checks the release-store and uses the
--    'Button' from there (removing it from the store in the process).
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

-- | All the environment and state required for 'KeyHandler' operations.
data KeyHandler = KeyHandler
  { _khST      :: !(MVar KhST) }
makeClassy ''KeyHandler

-- | Create a 'KeyHandler' object from a 'Keymap'
mkKeyHandler :: MonadIO m
  => Keymap ButtonCfg        -- ^ A 'Keymap' of all the button configurations
  -> m KeyHandler            -- ^ The action that creates a 'KeyHandler'
mkKeyHandler km = do
  bs <- initButtons km
  KeyHandler <$> newMVar (KhST bs M.empty M.empty)
  where initButtons = undefined

-- | A helper datatype just used to extract the action to perform from the
-- state. We extract the action from the state before running anything so we can
-- put the state MVar back before initiating the button action.
data HandleAction
  = WithButton   Button
  | WithPriority PriorityButton
  | DoNotHandle

-- | Find the appropriate handler by looking it up in a 'KhST' record. Return
-- both a 'HandleAction' describing what to do and a new, updated'KhST' record.
chooseHandler :: KeyAction -> KhST -> (KhST, HandleAction)
chooseHandler ka st = if
  -- If ka exists in priorityStore, pop it from the store
  | ka `M.member` (st^.priorityStore) ->
    let (a, ps) = pop ka $ st^.priorityStore
    in (st & priorityStore .~ ps, wrap WithPriority a)
  -- If ka is a Press, look it up in Keymap and store it in the releaseStore
  | isPress ka ->
    let b = st ^? keymap . atKey (ka ^. keycode)
    in (st & releaseStore . at (ka^.keycode) .~ b, wrap WithButton b)
  -- If ka is a Release, pop it from the releaseStore
  | True ->
    let (b, rs) = pop (ka^.keycode) $ st^.releaseStore
    in (st & releaseStore .~ rs, wrap WithButton b)
  where wrap = maybe DoNotHandle

-- | Handle a 'KeyAction' using the 'KeyHandler'
handleKey :: (HasKeyHandler e, HasLogFunc e)
  => KeyAction  -- ^ The 'KeyAction' to handle
  -> RIO e ()   -- ^ The resulting action
handleKey a = do
  logInfo $ "Handling: " <> (fromString . show $ a)
  view khST >>= flip modifyMVar (pure . chooseHandler a) >>= \case
    WithButton   b -> runButton a b
    WithPriority p -> runPriorityButton p
    DoNotHandle    -> pure ()


--------------------------------------------------------------------------------
-- $manip
--
-- We provide 2 actions to manipulate the the layer-stack. One that adds a layer
-- to the front, and one that pops the first occurence of a layer. Note that
-- both of these actions can throw errors.

-- | Push a layer to the front of the stack. This throws an error if the
-- provided 'Name' does not correspond to any map.
pushLayer :: (HasKeyHandler e, HasLogFunc e) => Name -> RIO e ()
pushLayer n =
  view khST >>= flip modifyMVar (pure . go) >>= \case
    Left e  -> do
      logError $ "Error pushing layer: " <> display n
      throwIO e
    Right _ ->
      logInfo $ "Pushed layer: " <> display n
  where go st = case pushLayer' n $ st^.keymap of
          Left e   -> (st, Left e)
          Right km -> (st & keymap .~ km, Right ())

-- | Pop a layer from the stack. This throws a 'LayerNotOnStackError' if the
-- layer does not exist on the stack, or a 'LayerNotFoundError' if the layer
-- does not exist in the keymap at all.
popLayer :: (HasKeyHandler e, HasLogFunc e) => Name -> RIO e ()
popLayer n =
  view khST >>= flip modifyMVar (pure . go) >>= \case
    Left e  -> do
      logError $ "Error popping layer: " <> display n
      throwIO e
    Right _ ->
      logInfo $ "Popped layer: " <> display n
  where go st = case popLayer' n $ st^.keymap of
          Left e   -> (st, Left e)
          Right km -> (st & keymap .~ km, Right ())
