-- |

module K.Initial.Util.Path
  ( PathRoot(..)
  , Path(..)
  , HasPath(..)
  , PathError
  , resolve
  )
where

import K.Initial.Util.Initial

import RIO.FilePath ((</>))
import System.FilePath.Glob (glob)
import UnliftIO.Directory

import qualified Control.Exception.Lens as Exc
import qualified RIO as S (unlines)

-- basic types -----------------------------------------------------------------

-- | Root directories we know how to search
data PathRoot
  = XdgCfg          -- ^ "kmonad" subdirectory relative to XdgConfig
  | Home            -- ^ Home directory
  | Custom FilePath -- ^ Any other path-prefix, may contain globs
  deriving (Eq, Show)

-- | How to look for a particular filepath
data Path = Path
  { _val    :: Text           -- ^ The pattern to match
  , _root   :: Maybe PathRoot -- ^ Optionally a path to be relative to
  , _doGlob :: Bool           -- ^ Whether to glob-match this expression
  } deriving (Eq, Show)
makeClassy ''Path

-- errors ----------------------------------------------------------------------

-- | Things that can go wrong in 'Path' resolution
data PathError
  = PathNoMatches Path                  -- ^ A 'Path' matched no files
  | PathMultipleMatches Path [FilePath] -- ^ A 'Path' matched multiple files
makeClassyPrisms ''PathError

instance Show PathError where
  show (PathNoMatches p) =
    "Path <" <> show p <> "> matches 0 files"
  show (PathMultipleMatches p fs)  = S.unlines $
    ("Path <" <> show p <> "> matches multiple files:"):fs

instance Exception PathError
instance AsPathError SomeException where _PathError = Exc.exception

-- ops -------------------------------------------------------------------------

-- | Resolve a 'Path' value to an absolute 'FilePath'
--
-- In addition to normal 'IOException's this can throw 'PathError's.
resolve :: MonadIO m => Path -> m FilePath
resolve p = do
  -- Construct the root
  r <- (</> (unpack $ p^.val)) <$> case p^.root of
    Nothing         -> pure ""
    Just XdgCfg     -> getXdgDirectory XdgConfig "kmonad"
    Just Home       -> getHomeDirectory
    Just (Custom f) -> pure f

  if not $ p^.doGlob
    -- When not globbing, check for file-existence
    then ifM (doesPathExist r) (pure r) (excThrowing _PathNoMatches p)

    -- When globbing, do the match
    else (liftIO . glob $ r) >>= \case
      [] -> excThrowing _PathNoMatches p
      [f] -> pure f
      fs  -> excThrowing _PathMultipleMatches (p, fs)
