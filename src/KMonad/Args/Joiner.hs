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

import KMonad.Model
import KMonad.Keyboard

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

import qualified RIO.NonEmpty as NE (cons, intersperse)
import qualified KMonad.Util.LayerStack  as L
import qualified RIO.HashMap      as M

--------------------------------------------------------------------------------
-- $err

-- | All the things that can go wrong with a joining attempt
data JoinError
  = DuplicateBlock   Text
  | MissingBlock     Text
  | DuplicateAlias   Text
  | DuplicateLayer   Text
  | DuplicateSource  (Maybe Text)
  | DuplicateKeyInSource (Maybe Text) [Keycode]
  | MissingAlias     Text
  | MissingLayer     Text
  | MissingSource    (Maybe Text)
  | MissingSetting   Text
  | DuplicateSetting Text
  | DuplicateLayerSetting Text Text
  | InvalidOS        Text
  | ImplArndDisabled
  | NestedTrans
  | InvalidComposeKey JoinError
  | LengthMismatch   Text Int Int
  | CmpSeqDisabled

instance Show JoinError where
  show e = case e of
    DuplicateBlock    t   -> "Encountered duplicate block of type: " <> unpack t
    MissingBlock      t   -> "Missing at least 1 block of type: "    <> unpack t
    DuplicateAlias    t   -> "Multiple aliases of the same name: "   <> unpack t
    DuplicateLayer    t   -> "Multiple layers of the same name: "    <> unpack t
    DuplicateSource   t   -> case t of
      Just t' -> "Multiple sources of the same name: " <> unpack t'
      Nothing -> "Multiple default sources"
    DuplicateKeyInSource   t ks   -> case t of
      Just t' -> "Keycodes appear multiple times in source `" <> unpack t' <> "`:" <> ((' ' :) . show =<< ks)
      Nothing -> "Keycodes appear multiple times in default source: " <> ((' ' :) . show =<< ks)
    MissingAlias      t   -> "Reference to non-existent alias: "     <> unpack t
    MissingLayer      t   -> "Reference to non-existent layer: "     <> unpack t
    MissingSource     t   -> case t of
      Just t' -> "Reference to non-existent source: " <> unpack t'
      Nothing -> "Reference to non-existent default source"
    MissingSetting    t   -> "Missing setting in 'defcfg': "         <> unpack t
    DuplicateSetting  t   -> "Duplicate setting in 'defcfg': "       <> unpack t
    DuplicateLayerSetting t s -> "Duplicate setting in 'deflayer '"  <> unpack t <> "': " <> unpack s
    InvalidOS         t   -> "Not available under this OS: "         <> unpack t
    ImplArndDisabled      -> "Implicit around via `A` or `S-a` are disabled in your config"
    NestedTrans           -> "Encountered 'Transparent' ouside of top-level layer"
    InvalidComposeKey err -> "Encountered invalid button as Compose key: " <> show err
    LengthMismatch t l s  -> mconcat
      [ "Mismatch between length of 'defsrc' and deflayer <", unpack t, ">\n"
      , "Source length: ", show s, "\n"
      , "Layer length: ", show l ]
    CmpSeqDisabled -> "Compose sequences are disabled in this context"


instance Exception JoinError

getDelay :: (Num a, Eq a) => a -> [a] -> Maybe a
getDelay def = nodelay . fromMaybe def . preview _head
 where
  nodelay 0 = Nothing
  nodelay x = Just x

-- | Join an entire 'CfgToken' from the current list of 'KExpr'.
joinConfig :: PCfg -> Either JoinError TCfg
joinConfig PCfg{_source       = []       } = throwError $ MissingSetting   "input"
joinConfig PCfg{_source       = _ : _ : _} = throwError $ DuplicateSetting "input"
joinConfig PCfg{_sink         = []       } = throwError $ MissingSetting   "output"
joinConfig PCfg{_sink         = _ : _ : _} = throwError $ DuplicateSetting "output"
joinConfig PCfg{_cmpKey       = _ : _ : _} = throwError $ DuplicateSetting "cmp-seq"
joinConfig PCfg{_cmpSeqDelay  = _ : _ : _} = throwError $ DuplicateSetting "cmp-seq-delay"
joinConfig PCfg{_fallThrough  = _ : _ : _} = throwError $ DuplicateSetting "fallthrough"
joinConfig PCfg{_allowCmd     = _ : _ : _} = throwError $ DuplicateSetting "allow-cmd"
joinConfig PCfg{_keySeqDelay  = _ : _ : _} = throwError $ DuplicateSetting "key-seq-delay"
joinConfig PCfg{_implArnd     = _ : _ : _} = throwError $ DuplicateSetting "implicit-around"
joinConfig
  PCfg
    { _source = [i]
    , _sink = [o]
    , _cmpKey = fromMaybe (KEmit KeyRightAlt) . preview _head -> ck
    , _cmpSeqDelay = getDelay 0 -> csd
    , _keymap = ks
    , _fallThrough = fromMaybe False . preview _head -> ft
    , _allowCmd = fromMaybe False . preview _head -> ac
    , _keySeqDelay = getDelay 1 -> ksd
    , _implArnd = fromMaybe IAAround . preview _head -> ia
    } = (`runJ` JCfg Nothing csd ks ia) $ do

    let ck' = maybe (throwError NestedTrans) pure =<< joinButton [] mempty ck
    ck'' <- either (throwError . InvalidComposeKey) (pure . Just) =<< asks (runJ ck')

    local (set cmpKey ck'') $ do
      i' <- pickInput i
      o' <- pickOutput o

      -- Extract the other blocks and join them into a keymap
      let als = extract _KDefAlias ks
      let lys = extract _KDefLayer ks
      let srcs = extract _KDefSrc ks
      (km, fl) <- joinKeymap srcs als lys

      pure $ TCfg i' o' km fl ft ac ksd

-- | Monad in which we join, just Except over Reader
newtype J a = J { unJ :: ExceptT JoinError (Reader JCfg) a }
  deriving newtype (Functor, Applicative, Monad, MonadError JoinError, MonadReader JCfg)

-- | Perform a joining computation
runJ :: J a -> JCfg -> Either JoinError a
runJ j = runReader (runExceptT $ unJ j)

--------------------------------------------------------------------------------
-- $full

-- | Like 'joinConfig' but throw the encountered error.
joinConfigIO :: HasLogFunc e => PCfg -> RIO e TCfg
joinConfigIO (joinConfig -> Left  e) = throwM e
joinConfigIO (joinConfig -> Right c) = pure c

-- | Extract anything matching a particular prism from a list
extract :: Prism' a b -> [a] -> [b]
extract p = mapMaybe (preview p)

--------------------------------------------------------------------------------
-- $settings

-- | Turn a 'HasLogFunc'-only RIO into a function from LogFunc to IO
lfToJ :: HasLogFunc lf => RIO lf a -> J (lf -> IO a)
lfToJ = pure . flip runRIO

#ifdef linux_HOST_OS

-- | The Linux correspondence between IToken and actual code
pickInput :: IToken -> J (LogFunc -> IO (Acquire KeySource))
pickInput (KDeviceSource f)   = lfToJ (deviceSource64 f)
pickInput KLowLevelHookSource = throwError $ InvalidOS "LowLevelHookSource"
pickInput (KIOKitSource _)    = throwError $ InvalidOS "IOKitSource"

-- | The Linux correspondence between OToken and actual code
pickOutput :: OToken -> J (LogFunc -> IO (Acquire KeySink))
pickOutput (KUinputSink t init) = lfToJ (uinputSink cfg')
  where cfg' = defUinputCfg { _keyboardName = unpack t
                            , _postInit     = unpack <$> init }
pickOutput (KSendEventSink _)   = throwError $ InvalidOS "SendEventSink"
pickOutput KKextSink            = throwError $ InvalidOS "KextSink"

#endif

#ifdef mingw32_HOST_OS

-- | The Windows correspondence between IToken and actual code
pickInput :: IToken -> J (LogFunc -> IO (Acquire KeySource))
pickInput KLowLevelHookSource = lfToJ llHook
pickInput (KDeviceSource _)   = throwError $ InvalidOS "DeviceSource"
pickInput (KIOKitSource _)    = throwError $ InvalidOS "IOKitSource"

-- | The Windows correspondence between OToken and actual code
pickOutput :: OToken -> J (LogFunc -> IO (Acquire KeySink))
pickOutput (KSendEventSink di) = lfToJ (sendEventKeySink di)
pickOutput (KUinputSink _ _)   = throwError $ InvalidOS "UinputSink"
pickOutput KKextSink           = throwError $ InvalidOS "KextSink"

#endif

#ifdef darwin_HOST_OS

-- | The Mac correspondence between IToken and actual code
pickInput :: IToken -> J (LogFunc -> IO (Acquire KeySource))
pickInput (KIOKitSource name) = lfToJ (iokitSource (unpack <$> name))
pickInput (KDeviceSource _)   = throwError $ InvalidOS "DeviceSource"
pickInput KLowLevelHookSource = throwError $ InvalidOS "LowLevelHookSource"

-- | The Mac correspondence between OToken and actual code
pickOutput :: OToken -> J (LogFunc -> IO (Acquire KeySink))
pickOutput KKextSink            = lfToJ kextSink
pickOutput (KUinputSink _ _)    = throwError $ InvalidOS "UinputSink"
pickOutput (KSendEventSink _)   = throwError $ InvalidOS "SendEventSink"

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
          else flip (M.insert t) mp <$> unnest (joinButton ns mp b)

--------------------------------------------------------------------------------
-- $but

-- | Turn 'Nothing's (caused by joining a KTrans) into the appropriate error.
-- KTrans buttons may only occur in 'DefLayer' definitions.
unnest :: J (Maybe Button) -> J Button
unnest = (maybe (throwError NestedTrans) pure =<<)

fromImplArnd :: DefButton -> DefButton -> ImplArnd -> J DefButton
fromImplArnd _ _ IADisabled        = throwError ImplArndDisabled
fromImplArnd o i IAAround          = pure $ KAround o i
fromImplArnd o i IAAroundOnly      = pure $ KAroundOnly o i
fromImplArnd o i IAAroundWhenAlone = pure $ KAroundWhenAlone o i

-- | Turn a button token into an actual KMonad `Button` value
joinButton :: LNames -> Aliases -> DefButton -> J (Maybe Button)
joinButton ns als =

  -- Define some utility functions
  let ret    = pure . Just
      go     = unnest . joinButton ns als
      jst    = fmap Just
      fi     = fromIntegral
      isps l = traverse go . maybe l ((`NE.intersperse` l) . KPause . fi)
  in \case
    -- Variable dereference
    KRef t -> case M.lookup t als of
      Nothing -> throwError $ MissingAlias t
      Just b  -> ret b

    -- Various simple buttons
    KEmit c -> ret $ emitB c
    KPressOnly c -> ret $ pressOnly c
    KReleaseOnly c -> ret $ releaseOnly c
    KCommand pr mbR -> ret $ cmdButton pr mbR
    KLayerToggle t -> if t `elem` ns
      then ret $ layerToggle t
      else throwError $ MissingLayer t
    KLayerSwitch t -> if t `elem` ns
      then ret $ layerSwitch t
      else throwError $ MissingLayer t
    KLayerAdd t -> if t `elem` ns
      then ret $ layerAdd t
      else throwError $ MissingLayer t
    KLayerRem t -> if t `elem` ns
      then ret $ layerRem t
      else throwError $ MissingLayer t
    KLayerDelay s t -> if t `elem` ns
      then ret $ layerDelay (fi s) t
      else throwError $ MissingLayer t
    KLayerNext t -> if t `elem` ns
      then ret $ layerNext t
      else throwError $ MissingLayer t

    -- Various compound buttons
    KComposeSeq bs     -> do csd <- view cmpSeqDelay
                             c   <- view cmpKey >>= \case
                               Just c  -> pure c
                               Nothing -> throwError CmpSeqDisabled
                             csd' <- for csd $ go . KPause . fi
                             jst $ tapMacro . NE.cons c . maybe id NE.cons csd' <$> isps bs csd
    KTapMacro bs mbD   -> jst $ tapMacro           <$> isps bs mbD
    KBeforeAfterNext b a -> jst $ beforeAfterNext <$> go b <*> go a
    KTapMacroRelease bs mbD ->
      jst $ tapMacroRelease           <$> isps bs mbD
    KAround o i        -> jst $ around             <$> go o <*> go i
    KTapNext t h       -> jst $ tapNext            <$> go t <*> go h
    KTapHold s t h     -> jst $ tapHold (fi s)     <$> go t <*> go h
    KTapHoldNext s t h mtb
      -> jst $ tapHoldNext (fi s) <$> go t <*> go h <*> traverse go mtb
    KTapNextRelease t h -> jst $ tapNextRelease    <$> go t <*> go h
    KTapHoldNextRelease ms t h mtb
      -> jst $ tapHoldNextRelease (fi ms) <$> go t <*> go h <*> traverse go mtb
    KTapNextPress t h  -> jst $ tapNextPress       <$> go t <*> go h
    KTapHoldNextPress ms t h mtb
      -> jst $ tapHoldNextPress (fi ms) <$> go t <*> go h <*> traverse go mtb
    KAroundOnly o i    -> jst $ aroundOnly         <$> go o <*> go i
    KAroundWhenAlone o i -> jst $ aroundWhenAlone  <$> go o <*> go i
    KAroundImplicit o i  -> joinButton ns als =<< fromImplArnd o i =<< view implArnd
    KAroundNext b      -> jst $ aroundNext         <$> go b
    KAroundNextSingle b -> jst $ aroundNextSingle <$> go b
    KAroundNextTimeout ms b t -> jst $ aroundNextTimeout (fi ms) <$> go b <*> go t
    KPause ms          -> jst . pure $ onPress (pause ms)
    KMultiTap bs d     -> jst $ multiTap <$> go d <*> mapM f bs
      where f (ms, b) = (fi ms,) <$> go b
    KStepped bs        -> jst $ steppedButton <$> mapM go bs
    KStickyKey s d     -> jst $ stickyKey (fi s) <$> go d

    -- Non-action buttons
    KTrans -> pure Nothing
    KBlock -> ret pass


--------------------------------------------------------------------------------
-- $src

type Sources = M.HashMap (Maybe Text) DefSrc

-- | Build up a hashmap of text to source mappings.
joinSources :: [DefSrc] -> J Sources
joinSources = foldM joiner mempty
  where
   joiner :: Sources -> DefSrc -> J Sources
   joiner sources src@DefSrc{ _srcName = n, _keycodes = ks }
     | n `M.member` sources = throwError $ DuplicateSource n
     | not (null dups)      = throwError $ DuplicateKeyInSource n dups
     | otherwise            = pure $ M.insert n src sources
    where
     dups :: [Keycode]
     dups = head <$> filter (not . null . tail) (groupAllWith id ks)

--------------------------------------------------------------------------------
-- $kmap

-- | Join the defsrc, defalias, and deflayer layers into a Keymap of buttons and
-- the name signifying the initial layer to load.
joinKeymap :: [DefSrc] -> [DefAlias] -> [DefLayer] -> J (LMap Button, LayerTag)
joinKeymap []   _   _          = throwError $ MissingBlock "defsrc"
joinKeymap _    _   []         = throwError $ MissingBlock "deflayer"
joinKeymap srcs als lys@(l1:_) = do
  let f acc x = if x `elem` acc then throwError $ DuplicateLayer x else pure (x:acc)
  nms   <- foldM f [] $ map _layerName lys     -- Extract all names
  als'  <- joinAliases nms als                 -- Join aliases into 1 hashmap
  srcs' <- joinSources  srcs                   -- Join all sources into 1 hashmap
  lys'  <- mapM (joinLayer als' nms srcs') lys -- Join all layers
  -- Return the layerstack and the name of the first layer
  pure (L.mkLayerStack lys', _layerName l1)

-- | Check and join 1 deflayer.
joinLayer ::
     Aliases                       -- ^ Mapping of names to buttons
  -> LNames                        -- ^ List of valid layer names
  -> Sources                       -- ^ Mapping of names to source layer
  -> DefLayer                      -- ^ The layer token to join
  -> J (Text, [(Keycode, Button)]) -- ^ The resulting tuple
joinLayer _ _ _ (DefLayer n DefLayerSettings{_lSrcName  = _ : _ : _}) = throwError $ DuplicateLayerSetting n "source"
joinLayer _ _ _ (DefLayer n DefLayerSettings{_lImplArnd = _ : _ : _}) = throwError $ DuplicateLayerSetting n "implicit-around"
joinLayer als ns srcs (DefLayer n settings) = do
  let bs = settings^.lButtons
  let assocSrc = settings^?lSrcName._head
  let implAround = settings^?lImplArnd._head

  src <- case M.lookup assocSrc srcs of
    Just src -> pure $ src^.keycodes
    Nothing  -> throwError $ MissingSource assocSrc
  -- Ensure length-match between src and buttons
  when (length bs /= length src) $
    throwError $ LengthMismatch n (length bs) (length src)

  -- Join each button and add it (filtering out KTrans)
  let f acc (kc, b) = joinButton ns als b >>= \case
        Nothing -> pure acc
        Just b' -> pure $ (kc, b') : acc
  maybe id (local . set implArnd) implAround $
    (n,) <$> foldM f [] (zip src bs)

--------------------------------------------------------------------------------
-- $test

-- fname :: String
-- fname = "/home/david/prj/hask/kmonad/doc/example.kbd"

-- test :: IO (J DefCfg)
-- test = runRIO () . fmap joinConfig $ loadTokens fname
