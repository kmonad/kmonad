{- | TODO: insert header

A simple way of encoding a 'Cmd' in 'Text'.

A Cmd expression follows one of the following patterns:
@cmd:literal command to be passed to shell@
@exec:cmdname:["arg1", "arg2"]@
@pass:@

Here:
  @cmd:@  - uses 'System.Process.shell'
  @exec:@ - uses 'System.Process.proc'
  @pass:@ - does nothing

Using @exec:@ should lead to better name-lookup and an easier time providing
arguments, but uses a more clunky syntax.

-}

module K.Shell.Cfg.Expr.Cmd where

import K.Shell.Cfg.Expr.Initial

-- | The 'Expr' correspondence between 'Cmd' and 'Text'
cmdExpr :: Expr Cmd
cmdExpr = customExpr "Cmd" cmdT cmdP

-- | A parser that tries to extract a 'Cmd' from 'Text'
--
-- Patterns
-- - "cmd:evtest this thing for me"
-- - "exec:rm:["/*", "-rf"]"
cmdP :: Parser Cmd
cmdP = choice
  [ string "cmd:"  *> (SimpleCmd <$> takeRest)
  , string "exec:" *> (CompoundCmd <$> exe <*> args)
  , string "pass:" $> PassCmd
  ]
  where
    exe  = unpack <$> takeWhile1P Nothing (/= ':') <* char ':'
    args = listOfP textP

-- | Create the textual representation of a 'Cmd'
cmdT :: Printer Cmd
cmdT (SimpleCmd t) = "cmd:" <> t
cmdT (CompoundCmd e a) = "exec:" <> pack e <> ":" <> tshow a
cmdT PassCmd = "pass:"
