{-# LANGUAGE DeriveAnyClass #-}
{-| TODO: do this

-}
module K.Shell.Cfg.CfgFile where

import K.Initial.Parsing

import K.Shell.Cfg.Initial
import K.Shell.Cfg.Cfgable
import K.Shell.Cfg.Expr
import K.Shell.Cfg.Default

import qualified Dhall as D
import qualified RIO.List as L
import qualified RIO.Text as T
import qualified RIO.HashMap as M

import qualified Control.Monad.Error.Lens as Err
import qualified Control.Exception.Lens as Exc

-- dhall types -----------------------------------------------------------------

-- | A label following valid Dhall syntax (this is just a annotation)
type DLabel = Label

-- | Text following the syntax of a valid gesture expression
type GestureExpr = Text

-- | An entry in a Dhall Map
data DEntry a = DEntry
  { mapKey :: DLabel
  , mapValue :: a
  } deriving (Generic, D.FromDhall, Show)

-- | A Dhall Map value
type DMap a = [ DEntry a ]

-- | Use '_DMap' as a view of an alist as a DMap, and 'from _DMap' as its inverse
_DMap :: Iso' [(Label, a)] (DMap a)
_DMap = iso (map $ uncurry DEntry) (map $ \(DEntry k v) -> (k, v))

instance HasNames (DMap a) where names = from _DMap . folded . _1

-- | Dhall doesn't allow numeric keys, so keycodes are encoded as "_xxx" strings.
type DCode = Text

-- | The types we expect to import directly from Dhall
--
-- Note that this determines the type of the final configuration structure we
-- expect to import from dhall. The only difference is that we strip the leading
-- underscores.
data DCfg = DCfg
  { _keycodes :: DMap Keycode     -- ^ DLabel to keycode-number
  , _kaliases :: DMap [Label]     -- ^ DLabel to list-of-aliases
  , _gestures :: DMap GestureExpr -- ^ DLabel to gesture expression
  , _galiases :: DMap [Label]     -- ^ DLabel to list-of-aliases
  , _options  :: DMap Text        -- ^ Name to option expression alist
  , _flags    :: [Text]           -- ^ List of flag expressions
  } deriving (Generic, D.FromDhall, Show)
makeLenses ''DCfg

-- basic types -----------------------------------------------------------------

-- | The CfgFile object exposed from the module
newtype CfgFile = CfgFile { _cfgFile :: Change ShellCfg }
makeLenses ''CfgFile

instance HasChange CfgFile ShellCfg where change = cfgFile

-- errors ----------------------------------------------------------------------

data CfgFileError
  = CfgNameError      NameError    -- ^ Either an empty name or duplicate names
  | CfgExprError      ExprError    -- ^ Error while parsing an Expr somewhere
  | CfgUnknownFlag    Name         -- ^ Reference to unknown flag
  | CfgUnknownOption  Name         -- ^ Reference to unknown option
  | CfgMissingKeyname Name         -- ^ Reference to non-existent keyname in alias or gesture
  | CfgMissingGesture Name         -- ^ Reference to non-existent gesture in alias
  | CfgGestureError   GestureError -- ^ Config contains some invalid gesture
makeClassyPrisms ''CfgFileError

instance Show CfgFileError where
  show (CfgNameError e) =
    "NameError while reading CfgFile: " <> show e
  show (CfgExprError e) =
    "ExprError while reading CfgFile: " <> show e
  show (CfgUnknownFlag n) =
    "Unknown flag: " <> unpack n
  show (CfgUnknownOption n) =
    "Unknown option: " <> unpack n
  show (CfgMissingKeyname n) =
    "Reference to undefined keyname in CfgFile: " <> show n
  show (CfgGestureError e) =
    "GestureError while reading CfgFile: " <> show e

instance Exception CfgFileError
instance AsCfgFileError SomeException where _CfgFileError = _SomeException
instance AsGestureError CfgFileError where _GestureError = _CfgGestureError
instance AsNameError CfgFileError where _NameError = _CfgNameError

type CanCfgFileError e m = (MonadError e m, AsCfgFileError e, AsNameError e)

-- IO --------------------------------------------------------------------------

-- | Try to load a 'CfgFile' from disk
loadCfgFile :: MonadIO m => Path -> m CfgFile
loadCfgFile p = do
  pth <- resolve p
  let opt = D.defaultInterpretOptions { D.fieldModifier = T.drop 1 }
  let dec = D.genericAutoWith opt
  d <- liftIO $ D.inputFile dec pth
  case validateCfgFile d of
    Left  e -> excThrowing _CfgFileError e
    Right x -> pure $ CfgFile x

-- validation ------------------------------------------------------------------

-- | Extract a validated 'Locale' from a 'DCfg' value
extractLocale :: CanLocale v r m => DCfg -> m Locale
extractLocale c = do



  undefined


-- -- | Try to read a Keycode from a DCode, throwing an error on fail
-- readDCode :: (CanCfgFileError e m) => DCode -> m Keycode
-- readDCode t = let err = errThrowing _CfgBadKeycode t in
--                 case unpack t of
--                   ('_':s) -> maybe err pure (readMaybe s)
--                   _ -> err

-- | Take all the fields in a 'CfgFile' and make a valid 'Change ShellCfg'
validateCfgFile :: (CanCfgFileError e m)
  => DCfg -> m (Change ShellCfg)
validateCfgFile c = do
  undefined
  -- -- Convert 'DMap DCode [Name]' to 'NameMap Keycode'
  -- let namedDCodes = c^..keycodes.from _DMap.to aflat.folded.swapped
  -- namedCodes <- mkNameMap =<< mapM (traverse readDCode) namedDCodes

  -- -- Ensure no overlap between keynames and gesture names
  -- case duplicates (namedCodes^..names <> c^..gestures.names) of
  --   [] -> pure ()
  --   ns -> errThrowing _DuplicateNames ns

  -- -- Extract and validate flags by looking them up and extracting their change
  -- fchange <- flip foldMapM (c^.flags) $ \flag -> do
  --   case lookupLong flag shellFlags of
  --     Nothing -> errThrowing _CfgUnknownFlag flag
  --     Just x  -> pure $ x^.change

  -- -- Extract and validate options by looking them up and calling their
  -- -- mkChange function on the provided Text value in the CfgFile
  -- ochange <- flip foldMapM (c^.options.from _DMap) $ \(okey, oval) -> do
  --   case lookupLong okey shellOptions of
  --     Nothing -> errThrowing _CfgUnknownOption okey
  --     Just x  -> case (x^.mkChange) oval of
  --       Left e -> errThrowing _CfgExprError e
  --       Right y -> pure y

  -- -- Extract all 'Toggles' expressions
  -- togTxt <- forM (c^.gestures.from _DMap) $ \(n, t) ->
  --   case decode togglesExpr t of
  --     Left e -> errThrowing _CfgExprError e
  --     Right x -> pure (n, x)

  -- -- Convert all 'Toggles Keyname' to 'Toggles Keycode'
  -- togNat <- forM togTxt $ \(n, t) ->
  --   case togglesMapMaybe (`M.lookup` namedCodes) t of
  --     Left n  -> errThrowing _CfgMissingKeyname n
  --     Right x -> pure (n, x)

  -- -- Convert all 'Toggles Keycode' to proper 'Gesture Keycode'
  -- gests <- forM togNat $ \(n, t) -> case mkGesture t of
  --   Left e -> errThrowing _CfgGestureError e
  --   Right x -> pure (n, x)

  -- -- Gather all the codes and gestures into a LocaleCfg
  -- let loc = Locale
  --       { _namedCodes = namedCodes
  --       , _namedRaps  = M.fromList gests }

  -- -- Gather all LocaleCfg, options, and flags into 1 update to ShellCfg
  -- pure . mconcat $
  --   [ setVal locale loc "set keynames and gesturenames"
  --   , fchange
  --   , ochange ]
