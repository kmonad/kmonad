{-# LANGUAGE CPP #-}
{-|
Module      : KMonad.Args.Joiner
Description : The code that turns tokens into a DaemonCfg
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

We perform configuration parsing in 2 steps:
- 1. We turn the text-file into a token representation
- 2. We check the tokens and turn them into an AppCfg

This module covers step 2.

NOTE: This is where we make a distinction between operating systems.

-}
module KMonad.Args.Joiner
  ( joinConfigIO
  , joinConfig
  )
where

import KMonad.Prelude hiding (uncons)

import KMonad.Args.Types

import KMonad.Action
import KMonad.Button
import KMonad.Keyboard
import KMonad.Keyboard.IO

#ifdef linux_HOST_OS
import KMonad.Keyboard.IO.Linux.DeviceSource
import KMonad.Keyboard.IO.Linux.UinputSink
#endif

#ifdef mingw32_HOST_OS
import KMonad.Keyboard.IO.Windows.LowLevelHookSource
import KMonad.Keyboard.IO.Windows.SendEventSink
#endif

#ifdef darwin_HOST_OS
import KMonad.Keyboard.IO.Mac.IOKitSource
import KMonad.Keyboard.IO.Mac.KextSink
#endif

import Control.Monad.Except

import RIO.List (uncons, headMaybe)
import RIO.Partial (fromJust)
import qualified Data.LayerStack  as L
import qualified RIO.HashMap      as M
import qualified RIO.Text         as T

--------------------------------------------------------------------------------
-- $err

-- | All the things that can go wrong with a joining attempt
data JoinError
  = DuplicateBlock   Text
  | MissingBlock     Text
  | DuplicateAlias   Text
  | DuplicateLayer   Text
  | MissingAlias     Text
  | MissingLayer     Text
  | MissingSetting   Text
  | DuplicateSetting Text
  | InvalidOS        Text
  | NestedTrans
  | InvalidComposeKey
  | LengthMismatch   Text Int Int

instance Show JoinError where
  show e = case e of
    DuplicateBlock    t   -> "Encountered duplicate block of type: " <> T.unpack t
    MissingBlock      t   -> "Missing at least 1 block of type: "    <> T.unpack t
    DuplicateAlias    t   -> "Multiple aliases of the same name: "   <> T.unpack t
    DuplicateLayer    t   -> "Multiple layers of the same name: "    <> T.unpack t
    MissingAlias      t   -> "Reference to non-existent alias: "     <> T.unpack t
    MissingLayer      t   -> "Reference to non-existent layer: "     <> T.unpack t
    MissingSetting    t   -> "Missing setting in 'defcfg': "         <> T.unpack t
    DuplicateSetting  t   -> "Duplicate setting in 'defcfg': "       <> T.unpack t
    InvalidOS         t   -> "Not available under this OS: "         <> T.unpack t
    NestedTrans           -> "Encountered 'Transparent' ouside of top-level layer"
    InvalidComposeKey     -> "Encountered invalid button as Compose key"
    LengthMismatch t l s  -> mconcat
      [ "Mismatch between length of 'defsrc' and deflayer <", T.unpack t, ">\n"
      , "Source length: ", show s, "\n"
      , "Layer length: ", show l ]


instance Exception JoinError

-- | Joining Config
data JCfg = JCfg
  { _cmpKey  :: Button  -- ^ How to prefix compose-sequences
  , _kes     :: [KExpr] -- ^ The source expresions we operate on
  }
makeLenses ''JCfg

defJCfg :: [KExpr] ->JCfg
defJCfg = JCfg
  (emitB KeyRightAlt)

newtype J a = J { unJ :: ExceptT JoinError (Reader JCfg) a }
  deriving ( Functor, Applicative, Monad
           , MonadError JoinError , MonadReader JCfg)

runJ :: J a -> JCfg -> Either JoinError a
runJ j = runReader (runExceptT $ unJ j)

--------------------------------------------------------------------------------
-- $full

-- | Turn a list of KExpr into a CfgToken, throwing errors when encountered.
--
-- NOTE: We start joinConfig with the default JCfg, but joinConfig might locally
-- override settings by things it reads from the config itself.
joinConfigIO :: HasLogFunc e => [KExpr] -> RIO e CfgToken
joinConfigIO es = case runJ joinConfig $ defJCfg es of
  Left  e -> throwM e
  Right c -> pure c

-- | Extract anything matching a particular prism from a list
extract :: Prism' a b -> [a] -> [b]
extract p = catMaybes . map (preview p)

data SingletonError
  = None
  | Duplicate

-- | Take the head of a list, or else throw the appropriate error
onlyOne :: [a] -> Either SingletonError a
onlyOne xs = case uncons xs of
  Just (x, []) -> Right x
  Just _       -> Left Duplicate
  Nothing      -> Left None

-- | Take the one and only block matching the prism from the expressions
oneBlock :: Text -> Prism' KExpr a -> J a
oneBlock t l = onlyOne . extract l <$> view kes >>= \case
  Right x        -> pure x
  Left None      -> throwError $ MissingBlock t
  Left Duplicate -> throwError $ DuplicateBlock t

-- | Update the JCfg and then run the entire joining process
joinConfig :: J CfgToken
joinConfig = getOverride >>= \cfg -> (local (const cfg) joinConfig')

-- | Join an entire 'CfgToken' from the current list of 'KExpr'.
joinConfig' :: J CfgToken
joinConfig' = do

  es <- view kes

  -- Extract the IO settings
  i  <- getI
  o  <- getO
  ft <- getFT

  -- Extract the other blocks and join them into a keymap
  let als = extract _KDefAlias    $ es
  let lys = extract _KDefLayer    $ es
  src      <- oneBlock "defsrc" _KDefSrc
  (km, fl) <- joinKeymap src als lys

  pure $ CfgToken
    { _snk  = o
    , _src  = i
    , _km   = km
    , _fstL = fl
    , _flt  = ft
    }

--------------------------------------------------------------------------------
-- $settings

-- | Return a JCfg with all settings from defcfg applied to the env's JCfg
getOverride :: J JCfg
getOverride = do
  env <- ask
  cfg <- oneBlock "defcfg" _KDefCfg
  let getB = joinButton [] M.empty
  let go e v = case v of
        SCmpSeq b  -> getB b >>= maybe (throwError InvalidComposeKey)
                                       (\b' -> pure $ set cmpKey b' e)
        _ -> pure e
  foldM go env cfg

-- | Turn a 'HasLogFunc'-only RIO into a function from LogFunc to IO
runLF :: (forall e. HasLogFunc e => RIO e a) -> LogFunc -> IO a
runLF = flip runRIO


-- | Extract the KeySource-loader from the 'KExpr's
getI :: J (LogFunc -> IO (Acquire KeySource))
getI = do
  cfg <- oneBlock "defcfg" _KDefCfg
  case onlyOne . extract _SIToken $ cfg of
    Right i          -> pickInput i
    Left  None       -> throwError $ MissingSetting "input"
    Left  Duplicate  -> throwError $ DuplicateSetting "input"

-- | Extract the KeySource-loader from a 'KExpr's
getO :: J (LogFunc -> IO (Acquire KeySink))
getO = do
  cfg <- oneBlock "defcfg" _KDefCfg
  case onlyOne . extract _SOToken $ cfg of
    Right o         -> pickOutput o
    Left  None      -> throwError $ MissingSetting "input"
    Left  Duplicate -> throwError $ DuplicateSetting "input"

-- | Extract the fallthrough setting
getFT :: J Bool
getFT = do
  cfg <- oneBlock "defcfg" _KDefCfg
  case onlyOne. extract _SFallThrough $ cfg of
    Right b        -> pure b
    Left None      -> pure False
    Left Duplicate -> throwError $ DuplicateSetting "fallthrough"

#ifdef linux_HOST_OS

-- | The Linux correspondence between IToken and actual code
pickInput :: IToken -> J (LogFunc -> IO (Acquire KeySource))
pickInput (KDeviceSource f)   = pure $ runLF (deviceSource64 f)
pickInput KLowLevelHookSource = throwError $ InvalidOS "LowLevelHookSource"
pickInput (KIOKitSource _)    = throwError $ InvalidOS "IOKitSource"

-- | The Linux correspondence between OToken and actual code
pickOutput :: OToken -> J (LogFunc -> IO (Acquire KeySink))
pickOutput (KUinputSink t init) = pure $ runLF (uinputSink cfg)
  where cfg = defUinputCfg { _keyboardName = T.unpack t
                           , _postInit     = T.unpack <$> init }
pickOutput KSendEventSink       = throwError $ InvalidOS "SendEventSink"
pickOutput KKextSink            = throwError $ InvalidOS "KextSink"

#endif

#ifdef mingw32_HOST_OS

-- | The Windows correspondence between IToken and actual code
pickInput :: IToken -> J (LogFunc -> IO (Acquire KeySource))
pickInput KLowLevelHookSource = pure $ runLF llHook
pickInput (KDeviceSource _)   = throwError $ InvalidOS "DeviceSource"
pickInput (KIOKitSource _)    = throwError $ InvalidOS "IOKitSource"

-- | The Windows correspondence between OToken and actual code
pickOutput :: OToken -> J (LogFunc -> IO (Acquire KeySink))
pickOutput KSendEventSink    = pure $ runLF sendEventKeySink
pickOutput (KUinputSink _ _) = throwError $ InvalidOS "UinputSink"
pickOutput KKextSink         = throwError $ InvalidOS "KextSink"

#endif

#ifdef darwin_HOST_OS

-- | The Mac correspondence between IToken and actual code
pickInput :: IToken -> J (LogFunc -> IO (Acquire KeySource))
pickInput (KIOKitSource name) = pure $ runLF (iokitSource (T.unpack <$> name))
pickInput (KDeviceSource _)   = throwError $ InvalidOS "DeviceSource"
pickInput KLowLevelHookSource = throwError $ InvalidOS "LowLevelHookSource"

-- | The Mac correspondence between OToken and actual code
pickOutput :: OToken -> J (LogFunc -> IO (Acquire KeySink))
pickOutput KKextSink            = pure $ runLF kextSink
pickOutput (KUinputSink _ _)    = throwError $ InvalidOS "UinputSink"
pickOutput KSendEventSink       = throwError $ InvalidOS "SendEventSink"

#endif

--------------------------------------------------------------------------------
-- $als

type Aliases = M.HashMap Text Button
type LNames  = [Text]

-- | Build up a hashmap of text to button mappings
--
-- Aliases can refer back to buttons that occured before.
joinAliases :: LNames -> [DefAlias] -> J Aliases
joinAliases ns als = foldM f M.empty $ concat als
  where f mp (t, b) = if t `M.member` mp
          then throwError $ DuplicateAlias t
          else flip (M.insert t) mp <$> (unnest $ joinButton ns mp b)

--------------------------------------------------------------------------------
-- $but

-- | Turn 'Nothing's (caused by joining a KTrans) into the appropriate error.
-- KTrans buttons may only occur in 'DefLayer' definitions.
unnest :: J (Maybe Button) -> J Button
unnest = join . fmap (maybe (throwError NestedTrans) (pure . id))

-- | Turn a button token into an actual KMonad `Button` value
joinButton :: LNames -> Aliases -> DefButton -> J (Maybe Button)
joinButton ns als =

  -- Define some utility functions
  let ret    = pure . Just
      go     = unnest . joinButton ns als
      jst    = fmap Just
      fi     = fromIntegral
  in \case
    -- Variable dereference
    KRef t -> case M.lookup t als of
      Nothing -> throwError $ MissingAlias t
      Just b  -> ret b

    -- Various simple buttons
    KEmit c -> ret $ emitB c
    KLayerToggle t -> if t `elem` ns
      then ret $ layerToggle t
      else throwError $ MissingLayer t
    KLayerSwitch t -> if t `elem` ns
      then ret $ layerSwitch t
      else throwError $ MissingLayer t

    -- Various compound buttons
    KComposeSeq bs -> view cmpKey >>= \c -> jst $ tapMacro . (c:) <$> mapM go bs
    KTapMacro bs   -> jst $ tapMacro       <$> mapM go bs
    KAround o i    -> jst $ around         <$> go o <*> go i
    KTapNext t h   -> jst $ tapNext        <$> go t <*> go h
    KTapHold s t h -> jst $ tapHold (fi s) <$> go t <*> go h
    KAroundNext b  -> jst $ aroundNext     <$> go b
    KPause ms      -> jst . pure $ onPress (pause ms)
    KMultiTap bs d -> jst $ multiTap <$> go d <*> mapM f bs
      where f (ms, b) = (fi ms,) <$> go b

    -- Non-action buttons
    KTrans -> pure Nothing
    KBlock -> ret pass


--------------------------------------------------------------------------------
-- $kmap

-- | Join the defsrc, defalias, and deflayer layers into a Keymap of buttons and
-- the name signifying the initial layer to load.
joinKeymap :: DefSrc -> [DefAlias] -> [DefLayer] -> J (LMap Button, LayerTag)
joinKeymap _   _   []  = throwError $ MissingBlock "deflayer"
joinKeymap src als lys = do
  let f acc x = if x `elem` acc then throwError $ DuplicateLayer x else pure (x:acc)
  nms  <- foldM f [] $ map _layerName lys   -- Extract all names
  als' <- joinAliases nms als               -- Join aliases into 1 hashmap
  lys' <- mapM (joinLayer als' nms src) lys -- Join all layers
  -- Return the layerstack and the name of the first layer
  pure $ (L.mkLayerStack lys', _layerName . fromJust . headMaybe $ lys)

-- | Check and join 1 deflayer.
joinLayer ::
     Aliases                       -- ^ Mapping of names to buttons
  -> LNames                        -- ^ List of valid layer names
  -> DefSrc                        -- ^ Layout of the source layer
  -> DefLayer                      -- ^ The layer token to join
  -> J (Text, [(Keycode, Button)]) -- ^ The resulting tuple
joinLayer als ns src DefLayer{_layerName=n, _buttons=bs} = do

  -- Ensure length-match between src and buttons
  when (length bs /= length src) $
    throwError $ LengthMismatch n (length bs) (length src)

  -- Join each button and add it (filtering out KTrans)
  let f acc (kc, b) = joinButton ns als b >>= \case
        Nothing -> pure acc
        Just b' -> pure $ (kc, b') : acc
  (n,) <$> foldM f [] (zip src bs)


--------------------------------------------------------------------------------
-- $test

-- fname :: String
-- fname = "/home/david/prj/hask/kmonad/doc/example.kbd"

-- test :: IO (J DefCfg)
-- test = runRIO () . fmap joinConfig $ loadTokens fname
