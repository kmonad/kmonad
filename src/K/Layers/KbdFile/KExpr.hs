-- |

module K.Layers.KbdFile.KExpr where

import K.Layers.KbdFile.Initial

import qualified RIO.HashMap as M
import qualified RIO.Text as T


import K.Shell -- DEBUG


-- top-level expressions -------------------------------------------------------

kbdFileP :: Loc e m => ParserT m [KExpr Keycode]
kbdFileP = sc *> some kexprP <* eof

kexprP :: Loc e m => ParserT m (KExpr Keycode)
kexprP = choice [defsrcP, deflayerP, defaliasP, defpairsP]

defsrcP :: Loc e m => ParserT m (KExpr Keycode)
defsrcP = statement "defsrc" $ do
  n <- fromMaybe "" <$> lex (optional $ keywordP "name" nameP)
  c <- some (lex keycodeP)
  pure $ KSrc n c

deflayerP :: Loc e m => ParserT m (KExpr Keycode)
deflayerP = statement "deflayer" $ do
  n <- lex nameP
  s <- lex (optional $ keywordP "src" nameP)
  b <- many (lex butP)
  pure $ KLayer n s b

defpairsP :: Loc e m => ParserT m (KExpr Keycode)
defpairsP = statement "defpairs" $ do
  n <- lex nameP
  b <- many $ pairP (lex keycodeP) (lex butP)
  pure $ KPairs n b

defaliasP :: Loc e m => ParserT m (KExpr Keycode)
defaliasP = statement "defalias" $ do
  bs <- some $ pairP (lex nameP) (lex butP)
  pure $ KAlias bs

-- basic expr ------------------------------------------------------------------

-- | Run a parser between 2 sets of parentheses starting with a symbol
statement :: Text -> ParserT m a -> ParserT m a
statement t p = lex $ do
  try $ symbol "(" *> symbol t
  p <* symbol ")"

-- | Parse a LISP-like keyword of the form @:keyword value@
keywordP :: Text -> ParserT m a -> ParserT m a
keywordP kw p = symbol (":" <> kw) *> lex p
  <?> "Keyword: " <> T.unpack kw

-- | Parse a variable reference
derefP :: ParserT m Name
derefP = prefixP (char '@') *> nameP

-- simple expr -----------------------------------------------------------------

-- | Parse valid characters until white-space or a terminator
nameP :: ParserT m Text
nameP = T.pack <$> some (satisfy wordChar) <?> "name"
  where wordChar c = not (isSpace c || c `elem` ("()\"'," :: String))

-- | Parse a 'Keycode' by parsing a name from 'namedCodes' in the 'Locale'
keycodeP :: (Loc e m) => ParserT m Keycode
keycodeP = namedP . M.toList =<< view namedCodes <?> "keyname"

-- | Parse a 'Rap' by parsing a name from 'namedRaps' in the 'Locale'
rapP :: (Loc e m) => ParserT m Rap
rapP = namedP . M.toList =<< view namedRaps

-- buttons ---------------------------------------------------------------------

butP :: (Loc e m) => ParserT m (But Keycode)
butP = simpleBP <|> stateBP <?> "button"

-- | Parse a simple, non-keyword button
simpleBP :: (Loc e m) => ParserT m (But Keycode)
simpleBP = choice
  [ BEmit <$> keycodeP
  , BRef <$> derefP
  ]

-- | Parse a button from a full statement
stateBP :: Loc e m => ParserT m (But Keycode)
stateBP = choice . map (uncurry statement) $
  [ ("around", BAround <$> butP <*> butP)
  , ("press-only", BPressOnly <$> keycodeP)
  , ("release-only", BReleaseOnly <$> keycodeP)
  , ("multi-tap", BMultiTap <$> many ((,) <$> lex msP <*> lex butP) <*> butP)
  , ("tap-hold", BTapHold <$> lex msP <*> butP <*> butP)
  , ("tap-hold-next"
    , BTapHoldNext <$> lex msP <*> butP <*> butP
                   <*> optional (keywordP "timeout-button" butP))
  , ("tap-next-release"
    , BTapNextRelease <$> butP <*> butP)
  , ("tap-hold-next-release"
    , BTapHoldNextRelease <$> lex msP <*> butP <*> butP
                          <*> optional (keywordP "timeout-button" butP))
  , ("tap-next", BTapNext <$> butP <*> butP)
  , ("layer-toggle", BLayerToggle <$> lex nameP)
  , ("momentary-layer", BLayerToggle <$> lex nameP)
  , ("layer-switch", BLayerSwitch <$> lex nameP)
  , ("permanent-layer", BLayerSwitch <$> lex nameP)
  , ("layer-add", BLayerAdd <$> lex nameP)
  , ("layer-rem", BLayerRem <$> lex nameP)
  , ("layer-delay", BLayerDelay <$> lex msP <*> lex nameP)
  , ("layer-next", BLayerNext <$> lex nameP)
  , ("around-next", BAroundNext <$> butP)
  , ("before-after-next", BBeforeAfterNext <$> butP <*> butP)
  , ("around-next-timeout", BAroundNextTimeout <$> lex msP <*> butP <*> butP)
  , ("tap-macro"
    , BTapMacro <$> lex (some butP) <*> optional (keywordP "delay" msP))
  , ("tap-macro-release"
    , BTapMacroRelease <$> lex (some butP) <*> optional (keywordP "delay" msP))
  , ("cmd-button", BCommand <$> lex textP <*> optional (lex textP))
  , ("pause", BPause <$> msP)
  , ("sticky-key", BStickyKey <$> lex msP <*> butP)
  ]




-- DEBUG -----------------------------------------------------------------------


loadKbdFile :: (MonadReader e m, HasLocale e, MonadIO m)
  => Path -> m [KExpr Keycode] -- KeymapCfg
loadKbdFile p = do
  txt <- readFileUtf8 =<< resolve p

  parseT kbdFileP txt >>= \case
    Left e -> excThrowing _KbdParseError e
    Right xs -> pure xs -- validateKeymap xs -- pure x

tstParse :: IO ()
tstParse = inCtx (testIvkM "-t test") $ inCtx cfgM $ do
  -- view namedCodes >>= pPrint . L.sortBy (bigger `on` fst) . M.toList
  view kbdPath >>= loadKbdFile >>= pPrint
