module KMonad.Components.KeyHandler
  ( -- * The KeyHandler environment
    -- $keyh
    KeyHandler
  , Keymap
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

import Prelude

import KMonad.Button
import KMonad.Keyboard
import KMonad.Util

import qualified Data.LayerStack as Q
import qualified RIO.HashMap     as M

--------------------------------------------------------------------------------
-- $err
data KeymapError
  = LayerNotFoundError   Name
  | LayerNotOnStackError Name
  deriving Show

instance Exception KeymapError

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


type Keymap   a = Q.LayerStack Name Keycode a

-- | The stateful aspect of a 'KeyHandler'. This will all be stored in 1 'MVar'.
data KhST = KhST
  { _keymap        :: !(Keymap ButtonEnv)
    -- ^ The current mapping of keycodes to buttons
  , _releaseStore  :: !(M.HashMap Keycode ButtonEnv)
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
mkKeyHandler ::
     Keymap Button -- ^ A 'Keymap' of all the button configurations
  -> RIO e KeyHandler -- ^ The action that creates a 'KeyHandler'
mkKeyHandler km = do
  bs <- km & Q.items . traversed %%~ initButtonEnv
  KeyHandler <$> newMVar (KhST bs M.empty M.empty)

-- | Print out a representation of the KeyHandler state
-- displayKeyHandler :: HasKeyHandler e => RIO e ()
-- displayKeyHandler = view khST >>= \v -> withMVar v $ \st -> liftIO $ do
--   print $ st ^. keymap . stack
--   print . M.keys $ st ^. releaseStore


-- | A helper datatype just used to extract the action to perform from the
-- state. We extract the action from the state before running anything so we can
-- put the state MVar back before initiating the button action.
data HandleAction
  = WithButton   ButtonEnv
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
    let b   = st ^? keymap . Q.atKey (ka ^. keycode)
        st' = st & releaseStore . at (ka ^. keycode) .~ b
    in (st', wrap WithButton b)
  -- If ka is a Release, pop it from the releaseStore
  | True ->
    let (b, rs) = pop (ka^.keycode) $ st^.releaseStore
    in (st & releaseStore .~ rs, wrap WithButton b)
  where wrap = maybe DoNotHandle


-- | Handle a 'KeyAction' using the 'KeyHandler'
handleKey :: (HasKeyHandler e, HasLogFunc e)
  => KeyAction     -- ^ The 'KeyAction' to handle
  -> RIO e Action  -- ^ The resulting action
handleKey a = do
  logDebug $ "Handling: " <> (fromString . show $ a)
  h <- flip modifyMVar (pure . chooseHandler a) =<< view khST
  case h of
    WithButton   b -> runButton (a^.switchAction) b
    WithPriority p -> runPriorityButton p
    DoNotHandle    -> pure $ Action (pure ())


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
  where go st = case Q.pushLayer n $ st^.keymap of
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
  where go st = case Q.popLayer n $ st^.keymap of
          Left e   -> (st, Left e)
          Right km -> (st & keymap .~ km, Right ())
