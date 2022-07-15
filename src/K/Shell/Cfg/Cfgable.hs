-- |

module K.Shell.Cfg.Cfgable where
-- |


import K.Initial
import K.Initial.Parsing (ParseError)
import K.Shell.Cfg.Initial
import K.Shell.Cfg.Expr

import Data.Monoid
import Data.Foldable

import qualified Control.Monad.Error.Lens as Err

-- types -----------------------------------------------------------------------

-- | A modification to some structure
data Change s = Change
  { _notes :: [Text]
  , _endo  :: Endo s
  }
makeClassy ''Change

-- | Run a change on some structure
runChange :: HasChange c s => c -> s -> s
runChange = appEndo . view (change . endo)

-- | A view on some object after it's been changed
changed :: HasChange c s => c -> Getter s s
changed c = to $ runChange c


instance Semigroup (Change s) where
  a <> b = Change (a^.notes <> b^.notes) (a^.endo <> b^.endo)
instance Monoid (Change s) where
  mempty = Change [] (Endo id)

-- | Create a 'Change' that sets a 'Traversal' to some value
setVal :: Traversal' s a -> a -> Text -> Change s
setVal l a t = Change [t] (Endo $ \s -> set l a s)

-- | Different locations a setting may be provided
data CfgSource = FromInvoc | FromCfgFile | FromEither
makeClassyPrisms ''CfgSource

-- | Identifying information about some 'Cfgable'
data CfgTag = CfgTag
  { _longName  :: Name       -- ^ The full name of a 'Cfgable'
  , _shortName :: Maybe Char -- ^ Short character for invocation
  , _doc       :: Text       -- ^ Help text for invocation and in-code documentation
  , _source    :: CfgSource  -- ^ Where to allow specification
  }
makeClassy ''CfgTag

-- | Explicitly mark a configable as invoc only
invocOnly :: HasCfgTag a => a -> a
invocOnly = set source FromInvoc

-- | Explicitly mark a configable as cfg-file only
cfgFileOnly :: HasCfgTag a => a -> a
cfgFileOnly = set source FromCfgFile

-- | A fixed change to the 'ShellCfg'
data Flag s = Flag
  { _ftag :: CfgTag  -- ^ Identifying information
  , _fchange :: Change s   -- ^ Change to make to 'ShellCfg'
  }
makeClassy ''Flag

instance HasCfgTag (Flag s) where cfgTag = ftag
instance HasChange (Flag s) s where change = fchange

-- | Create a flag that is valid for both 'CfgSource's
mkFlag :: ()
  => Name       -- ^ Long name
  -> Maybe Char -- ^ Optional short name for invocation
  -> Text       -- ^ Help text
  -> Change s      -- ^ The change to make to a structure
  -> Flag s
mkFlag n c t = Flag (CfgTag n c t FromEither)

-- | A change to 'ShellCfg' that depends on some value.
data Option e s = Option
  { _otag  :: CfgTag        -- ^ Identifying information
  , _mkChange :: Text -> Either e (Change s) -- ^ How to construct a change to the 'ShellCfg'
  }
makeClassy ''Option

instance HasCfgTag (Option e s) where cfgTag = otag

-- | Create an option that is valid for both 'CfgSource's
mkOption :: ()
  => Name            -- ^ Long name
  -> Maybe Char      -- ^ Optional short name for invocation
  -> Text            -- ^ Help text
  -> (Text -> Either e (Change s)) -- ^ How to update the structure with the value
  -> Option e s
mkOption n c t = Option (CfgTag n c t FromEither)

-- | Create a function that sets some value using an 'Expr'
setWithExpr :: (Show a, MonadError e m, AsExprError e)
  => Traversal' s a
  -> Expr a
  -> Name
  -> (Text -> m (Change s))
setWithExpr l e n t = case decode e t of
  Left err -> errThrowing __ExprError err
  Right a  -> pure $ setVal l a ("set " <> n <> " to " <> tshow a)

-- util ------------------------------------------------------------------------

-- | Lookup a flag by its long name
lookupLong :: (Foldable f, HasCfgTag a) => Name -> f a -> Maybe a
lookupLong n = find (\f -> f^.longName == n)
