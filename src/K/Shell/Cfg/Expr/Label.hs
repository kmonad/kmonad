{- A bit of text used to identify things in cfg-files



-}
module K.Shell.Cfg.Expr.Label
where

import K.Shell.Cfg.Expr.Initial
import qualified RIO.Text as T


labelExpr :: Expr Label
labelExpr = customExpr "Label" id labelP

labelP :: Parser Label
labelP = fmap mconcat . some $ choice
  [ T.singleton <$> anySingleBut ';' <?> "non semicolon character"
  , T.singleton <$> satisfy (not . isSpace)
  , string "\\;"
  ] -- There is probably a better way than making many little texts first
