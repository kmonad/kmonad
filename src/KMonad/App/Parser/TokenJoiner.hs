{-# LANGUAGE ViewPatterns #-}
{-|
Module      : KMonad.App.Parser.TokenJoiner
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
module KMonad.App.Parser.TokenJoiner
  ( joinConfigIO
  , joinConfig
  )
where

import qualified Data.List as L 
  
import System.Directory (listDirectory)
import KMonad.Prelude hiding (uncons)

import KMonad.App.Parser.Types
import KMonad.App.KeyIO

import KMonad.Model.Types

import KMonad.Pullchain.Action
import KMonad.Pullchain.Button
import KMonad.Pullchain.Types
import KMonad.Util

import Control.Monad.Except

import RIO.List (uncons, headMaybe)
import RIO.Partial (fromJust)
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
  { _cmpKey  :: BCfg    -- ^ How to prefix compose-sequences
  , _kes     :: [KExpr] -- ^ The source expresions we operate on
  }
makeLenses ''JCfg

defJCfg :: [KExpr] ->JCfg
defJCfg = JCfg
  (BEmit $ kc "ralt")

-- | Monad in which we join, just Except over Reader
newtype J a = J { unJ :: ExceptT JoinError (Reader JCfg) a }
  deriving ( Functor, Applicative, Monad
           , MonadError JoinError , MonadReader JCfg )

-- | Perform a joining computation
runJ :: J a -> JCfg -> Either JoinError a
runJ = runReader . runExceptT . unJ 
--------------------------------------------------------------------------------
-- $full

-- | Turn a list of KExpr into a CfgToken, throwing errors when encountered.
--
-- NOTE: We start joinConfig with the default JCfg, but joinConfig might locally
-- override settings by things it reads from the config itself.
joinConfigIO :: HasLogFunc e => [KExpr] -> RIO e CfgToken
joinConfigIO = 
  pure . runJ joinConfig . defJCfg >=> either throwM pure 

-- | Extract anything matching a particular prism from a list
extract :: Prism' a b -> [a] -> [b]
extract p = mapMaybe (preview p)

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
  al <- getAllow

  -- Extract the other blocks and join them into a keymap
  let als = extract _KDefAlias    $ es
  let lys = extract _KDefLayer    $ es
  src      <- oneBlock "defsrc" _KDefSrc
  (km, fl) <- joinKeymap src als lys

  pure $ CfgToken
    { _snk   = o
    , _src   = i
    , _km    = km
    , _fstL  = fl
    , _flt   = ft
    , _allow = al
    }

--------------------------------------------------------------------------------
-- $settings
--
-- TODO: This needs to be seriously refactored: all this code duplication is a
-- sign that something is amiss.

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


-- | Extract the KeySource-loader from the 'KExpr's
getI :: J KeyInputCfg
getI = do
  cfg <- oneBlock "defcfg" _KDefCfg
  case onlyOne . extract _SIToken $ cfg of
    Right i          -> pickInput i
    Left  None       -> throwError $ MissingSetting "input"
    Left  Duplicate  -> throwError $ DuplicateSetting "input"

-- | Extract the KeySource-loader from a 'KExpr's
getO :: J KeyOutputCfg
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
  case onlyOne . extract _SFallThrough $ cfg of
    Right b        -> pure b
    Left None      -> pure False
    Left Duplicate -> throwError $ DuplicateSetting "fallthrough"

-- | Extract the fallthrough setting
getAllow :: J Bool
getAllow = do
  cfg <- oneBlock "defcfg" _KDefCfg
  case onlyOne . extract _SAllowCmd $ cfg of
    Right b        -> pure b
    Left None      -> pure False
    Left Duplicate -> throwError $ DuplicateSetting "allow-cmd"


pickInput :: IToken -> J KeyInputCfg
pickInput (KDeviceSource f)     = pure . LinuxEvdevCfg . EvdevCfg $ f
pickInput (KFindFirstWithFix fix) = case fix of
  Prefix pre -> pure . LinuxEvdevCfg . EvdevSearchPrefix pre $ dirs 
  Suffix pre -> pure . LinuxEvdevCfg . EvdevSearchSuffix pre $ dirs 
  where dirs = "/dev/input/by-id" :| ["/dev/input/by-path"]
pickInput (KIOKitSource _)      = pure . MacKIOKitCfg     $ KIOKitCfg
pickInput KLowLevelHookSource = pure . WindowsLLHookCfg $ LLHookCfg

pickOutput :: OToken -> J KeyOutputCfg
pickOutput (KUinputSink t init) = pure . LinuxUinputCfg
  $ def { _keyboardName = t
        , _postInit     = unpack <$> init }
pickOutput KKextSink      = pure . MacKextCfg          $ KextCfg
pickOutput KSendEventSink = pure . WindowsSendEventCfg $ SendEventCfg

--------------------------------------------------------------------------------
-- $als

type Aliases = M.HashMap Text BCfg
type LNames  = [Name]

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
unnest :: J (Maybe BCfg) -> J BCfg
unnest = join . fmap (maybe (throwError NestedTrans) (pure . id))

-- | Turn a 'DefButton' statement into a `BCfg`
--
-- This:
-- * Filters out Trans buttons
-- * Recursively dereferences all buttons (also those within compound buttons)
-- * Translates to a 'BCfg' from 'Model.Types'
--
-- NOTE: This is a bit of a stop-gap solution that causes a lot of
-- code-duplication. However, it seemed like the best way to start separating
-- parsing from model implementation. Ideally we'd figure out a way to parse
-- 'BCfg's directly from the configuration files, but that would currently make
-- dereferencing aliases a bit difficult.
--
-- The best solution to this problem *seems to me* to be implementing our
-- Parser's with state, so that we build up a dictionary of aliases as we
-- proceed. This would break the laziness of our parsing, but I'm fine losing
-- that on a major version change.
joinButton :: LNames -> Aliases -> DefButton -> J (Maybe BCfg)
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
    KEmit c -> ret $ BEmit c
    KCommand pr mbR -> ret $ BCommand pr mbR
    KLayerToggle t -> if t `elem` ns
      then ret $ BLayerToggle t
      else throwError $ MissingLayer t
    KLayerSwitch t -> if t `elem` ns
      then ret $ BLayerSwitch t
      else throwError $ MissingLayer t
    KLayerAdd t -> if t `elem` ns
      then ret $ BLayerAdd t
      else throwError $ MissingLayer t
    KLayerRem t -> if t `elem` ns
      then ret $ BLayerRem t
      else throwError $ MissingLayer t
    KLayerDelay s t -> if t `elem` ns
      then ret $ BLayerDelay (fi s) t
      else throwError $ MissingLayer t
    KLayerNext t -> if t `elem` ns
      then ret $ BLayerNext t
      else throwError $ MissingLayer t

    -- TODO: Make this its own button type eventually. Keeping 'composeKey'
    -- setting in the Parser is a bit silly, should just be a configurable app
    -- or model parameter.
    KComposeSeq bs     -> view cmpKey >>= \c -> jst $ BTapMacro . (c:) <$> mapM go bs

    -- Various compound buttons
    KTapMacro bs       -> jst $ BTapMacro <$> mapM go bs
    KTapMacroRelease bs -> jst $ BTapMacroRelease <$> mapM go bs
    KAround o i        -> jst $ BAround <$> go o <*> go i
    KTapNext t h       -> jst $ BTapNext <$> go t <*> go h
    KTapHold s t h     -> jst $ BTapHold (fi s)     <$> go t <*> go h
    KTapHoldNext s t h -> jst $ BTapHoldNext (fi s) <$> go t <*> go h
    KTapNextRelease t h -> jst $ BTapNextRelease <$> go t <*> go h
    KTapHoldNextRelease ms t h
      -> jst $ BTapHoldNextRelease (fi ms) <$> go t <*> go h
    KAroundNext b      -> jst $ BAroundNext <$> go b
    KAroundNextSingle b -> jst $ BAroundNextSingle <$> go b
    KMultiTap bs d     -> jst $ BMultiTap <$> mapM f bs <*> go d
      where f (ms, b) = (fi ms,) <$> go b
    KStickyKey s d     -> jst $ BStickyKey (fi s) <$> go d

    KBlock -> ret BBlock
    KPause ms          -> ret $ BPause ms

    -- Filter out all 'KTrans' statements
    KTrans -> pure Nothing


--------------------------------------------------------------------------------
-- $kmap

-- | Join the defsrc, defalias, and deflayer layers into a Keymap of buttons and
-- the name signifying the initial layer to load.
joinKeymap :: DefSrc -> [DefAlias] -> [DefLayer] -> J (Keymap BCfg, Name)
joinKeymap _   _   []  = throwError $ MissingBlock "deflayer"
joinKeymap src als lys = do
  let f acc x = if x `elem` acc then throwError $ DuplicateLayer x else pure (x:acc)
  nms  <- foldM f [] $ map _layerName lys   -- Extract all names
  als' <- joinAliases nms als               -- Join aliases into 1 hashmap
  lys' <- mapM (joinLayer als' nms src) lys -- Join all layers
  -- Return the layerstack and the name of the first layer
  pure $ ( map (second M.fromList) $ lys'
         , _layerName . fromJust . headMaybe   $ lys)

-- | Check and join 1 deflayer.
joinLayer ::
     Aliases                     -- ^ Mapping of names to buttons
  -> LNames                      -- ^ List of valid layer names
  -> DefSrc                      -- ^ Layout of the source layer
  -> DefLayer                    -- ^ The layer token to join
  -> J (Text, [(Keycode, BCfg)]) -- ^ The resulting tuple
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
