-- |

module K.Layers.KbdFile

where

import K.Layers.KbdFile.Initial as X
import K.Layers.KbdFile.KExpr as X

-- import K.Initial.Parsing
-- import K.Layers.Initial
-- import K.Keyboard

-- TEMP

-- import Data.Char

-- import qualified RIO.List as L
-- import qualified RIO.HashMap as M




-- loadKbdFile :: (MonadReader e m, HasLocale e, MonadIO m)
--   => Path -> m [KExpr Keycode] -- KeymapCfg
-- loadKbdFile p = do
--   txt <- readFileUtf8 =<< resolve p

--   parseT kbdFileP txt >>= \case
--     Left e -> excThrowing _KbdParseError e
--     Right xs -> pure xs -- validateKeymap xs -- pure x

-- -- DEBUG -----------------------------------------------------------------------


-- tstParse :: IO ()
-- tstParse = inCtx (testIvkM "-t test") $ inCtx cfgM $ do
--   -- view namedCodes >>= pPrint . L.sortBy (bigger `on` fst) . M.toList
--   view kbdPath >>= loadKbdFile >>= pPrint
