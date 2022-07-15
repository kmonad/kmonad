{- | TODO: insert header

A simple way of encoding a 'Path' in 'Text'.

A Path expression follows the following pattern:
@[[*][x,~]:]rest/of/the/path@

Here:
  @*@ - activates glob-expansion
  @x@ - sets the root relative to $XDGCONFIG/kmonad
  @~@ - sets the root relative to $HOME
  @:@ - signals the end of the preamble flags

To include a literal @:@ in your path, use @::@.

Examples of 'pathExpr':
  @*x:*.kbd@    - with globbing, in xdgconfig, the @*.kbd@ pattern
  @~:thing.txt@ - without globbing, in home, the @thing.txt@ pattern
  @:thing.txt@  - without globbing, in root, the @thing.txt@ pattern
  @thing.txt@   - same as above, without flags the colon may be left out

Other 'Expr Path' may be generated with 'mkPathExpr' that include other
character-to-relative-root bindings. See "K.Shell.Cfg.Expr.KeyInput" for an
example.

-}
module K.Shell.Cfg.Expr.Path
  ( Roots
  , pathExpr
  , mkPathExpr
  )
where

import K.Shell.Cfg.Expr.Initial

import qualified RIO.Text as T
import RIO.Text.Partial (replace)

-- basic values ----------------------------------------------------------------

-- | 'Roots' are a collection of 'Char' to 'PathRoot' correspondences
type Roots = [(Char, PathRoot)]

-- | 'PathRoot's that are available to all 'Path' parsers
defRoots :: Roots
defRoots = [('x', XdgCfg), ('~', Home)]

-- | Create a list of unique 'Char' 'PathRoot' correspondences
mkRoots :: Roots -> Roots
mkRoots rs = let xs = rs <> defRoots in case duplicates $ xs^..folded._1 of
  [] -> xs
  _  -> devFail $ "Overlapping 'Char's in Path Expr: " <> tshow xs

-- | Create a custom 'Path' expression with custom roots
mkPathExpr :: ExprType -> Roots -> Expr Path
mkPathExpr t rs =  customExpr t (pathT rs) (pathP rs)

-- | A simple 'Path' 'Expr' without extra roots
pathExpr :: Expr Path
pathExpr = mkPathExpr "Path" []

-- parser ----------------------------------------------------------------------

-- | Parse a short preamble consisting of initial characters followed by a colon
preamble :: Roots -> Parser (Bool, Maybe PathRoot)
preamble cr = do
  let rs = mkRoots cr
  g <- isJust <$> optional (char '*')
  r <- optional $ oneOf (rs^..folded._1)
  _ <- char ':'
  pure (g, r >>= (`lookup` rs))

-- | Create a 'Parser' for 'Path's, possibly with extra 'Roots'
pathP :: Roots -> Parser Path
pathP rs = do
  pre <- fromMaybe (False, Nothing) <$> (optional . try $ preamble rs)
  rst <- exprTail
  pure $ Path rst (pre^._2) (pre^._1)

-- printer ---------------------------------------------------------------------

-- | Encode a 'Path' as 'Text'
pathT :: Roots -> Printer Path
pathT cr p =
  let g = if p^.doGlob then "*" else ""
      x = case p^.root of
        Nothing -> ""
        Just r  -> case lookup r (mkRoots cr^..folded.swapped) of
          Nothing -> devFail $ "Unnamed pathroot: " <> tshow (p^.root)
          Just c -> T.singleton c
  in g <> x <> ":" <> replace ":" "::" (p^.val)
