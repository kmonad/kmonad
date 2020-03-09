{-|
Module      : KLisp.Joiner
Description : The code that turns tokens into a DaemonCfg
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

-}
module KList.Joiner

where

import KPrelude hiding (uncons)

import KLisp.Parser
import KLisp.Types

import KMonad
import KMonad.Daemon
import KMonad.Keyboard.IO
import KMonad.Keyboard.IO.Linux.DeviceSource
import KMonad.Keyboard.IO.Linux.UinputSink

import RIO.List (uncons)
import qualified Data.LayerStack  as L
import qualified RIO.HashMap      as M
import qualified RIO.Text         as T

--------------------------------------------------------------------------------
-- $err

data JoinError
  = DuplicateBlock Text
  | MissingBlock   Text
  | DuplicateAlias Text
  | DuplicateLayer Text
  | LengthMismatch Text Int Int
  | MissingAlias   Text
  | MissingLayer   Text
  | NestedTrans
  deriving Show

instance Exception JoinError

type J a = Either JoinError a

--------------------------------------------------------------------------------
-- $full

-- | Extract anything matching a particular prism from a list
extract :: Prism' a b -> [a] -> [b]
extract p = catMaybes . map (preview p)

-- | Parse an entire DaemonCfg from a list of KExpr
joinConfig :: [KExpr] -> J DefCfg
joinConfig es = do

  -- Extract exactly 1 item from a list, otherwise throw the appropriate error
  let onlyOne t xs = case uncons xs of
        Just (x, []) -> pure x
        Just _       -> Left $ DuplicateBlock t
        Nothing      -> Left $ MissingBlock   t

  -- Extract and join the `defio` block
  dio    <- onlyOne "defio" . extract _KDefIO $ es
  (i, o) <- (,) <$> getI dio <*> getO dio

  -- Extract the other blocks and join them into a keymap
  let als = extract _KDefAlias $ es
  let lys = extract _KDefLayer $ es
  src      <- onlyOne "defsrc" $ extract _KDefSrc $ es
  (km, fl) <- joinKeymap src als lys

  pure $ DefCfg
    { _snk  = o
    , _src  = i
    , _km   = km
    , _fstL = fl
    , _port = ()
    }

--------------------------------------------------------------------------------
-- $io

-- | Turn a 'HasLogFunc'-only RIO into a function from LogFunc to IO
runLF :: (forall e. HasLogFunc e => RIO e a) -> LogFunc -> IO a
runLF = flip runRIO

-- | Extract the KeySource-loader from a `DefIO`
getI :: DefIO -> J (LogFunc -> IO (Acquire KeySource))
getI dio = case _itoken dio of
  KDeviceSource pth -> pure $ runLF (deviceSource64 pth)

-- | Extract the KeySink-loader from a `DefIO`
getO :: DefIO -> J (LogFunc -> IO (Acquire KeySink))
getO dio = case _otoken dio of
  KUinputSink t init -> pure $ runLF (uinputSink
    (defUinputCfg { _keyboardName = T.unpack t
                  , _postInit     = T.unpack <$> init}))


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
          then Left $ DuplicateAlias t
          else flip (M.insert t) mp <$> (unnest $ joinButton ns mp b)

--------------------------------------------------------------------------------
-- $but

-- | Turn 'Nothing's (caused by joining a KTrans) into the appropriate error.
-- KTrans buttons may only occur in 'DefLayer' definitions.
unnest :: J (Maybe Button) -> J Button
unnest = join . fmap (maybe (Left NestedTrans) (Right . id))

-- | Turn a button token into an actual KMonad `Button` value
joinButton :: LNames -> Aliases -> DefButton -> J (Maybe Button)
joinButton ns als =
  -- Define some utility functions
  let ret    = Right . Just
      go     = unnest . joinButton ns als
      jst    = fmap Just
      fi     = fromIntegral
  in \case

    -- Variable dereference
    KRef t -> case M.lookup t als of
      Nothing -> Left $ MissingAlias t
      Just b  -> ret b

    -- Various simple buttons
    KEmit c -> ret $ emitB c
    KLayerToggle t -> if t `elem` ns
      then ret $ layerToggle t
      else Left $ MissingLayer t

    -- Various compound buttons
    KTapMacro bs   -> jst $ tapMacro       <$> mapM go bs
    KAround o i    -> jst $ around         <$> go o <*> go i
    KTapNext t h   -> jst $ tapNext        <$> go t <*> go h
    KTapHold s t h -> jst $ tapHold (fi s) <$> go t <*> go h
    KMultiTap bs d -> jst $ multiTap <$> go d <*> mapM f bs
      where f (ms, b) = (fi ms,) <$> go b

    -- Non-action buttons
    KTrans -> Right Nothing
    KBlock -> ret pass


--------------------------------------------------------------------------------
-- $kmap

joinKeymap :: DefSrc -> [DefAlias] -> [DefLayer] -> J (Keymap Button, LayerTag)
joinKeymap _   _   []  = Left $ MissingBlock "deflayer"
joinKeymap src als lys = do

  let nms  = map _layerName lys
  amp <- joinAliases nms als

  -- Generate [(tag, [(Keycode Button)])] lists
  -- let g ls DefLayer{_layerName=n, _buttons=bs} = if
  --       | length bs /= nsrc      -> Left  $ LengthMismatch n (length bs) nsrc
  --       | True                   -> Right $ ls <> [(n, zip src bs)]
  undefined


joinLayer ::
     Aliases                       -- ^ Mapping of names to buttons
  -> LNames                        -- ^ List of valid layer names
  -> DefSrc                        -- ^ Layout of the source layer
  -> DefLayer                      -- ^ The layer token to join
  -> J (Text, [(Keycode, Button)]) -- ^ The resulting tuple
joinLayer als ns src DefLayer{_layerName=n, _buttons=bs} = do

  -- Ensure length-match between src and buttons
  when (length bs /= length src) $
    Left $ LengthMismatch n (length bs) (length src)

  -- Join each button and add it (filtering out KTrans)
  let f acc (kc, b) = joinButton ns als b >>= \case
        Nothing -> pure acc
        Just b' -> pure $ (kc, b') : acc
  (n,) <$> foldM f [] (zip src bs)



 

--------------------------------------------------------------------------------
-- $test

fname :: String
fname = "/home/david/prj/hask/kmonad/doc/example.kbd"

test :: IO (J DefCfg)
test = runRIO () . fmap joinConfig $ loadTokens fname
