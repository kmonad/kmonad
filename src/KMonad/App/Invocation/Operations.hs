module KMonad.App.Invocation.Operations

where

import KMonad.Prelude
import KMonad.App.Invocation.Types
import KMonad.App.Parser.Types

-- | Join the options given from the command line with the one read from the
-- configuration file.
-- This does not yet throw any kind of exception, as we are simply inserting the
-- given options into every 'KDefCfg' block that we see.
joinCLI :: Invoc -> [KExpr] -> [KExpr]
joinCLI cmd = traverse._KDefCfg %~ insertCliOption cliList
 where
  -- | All options and flags that were given on the command line.
  cliList :: DefSettings
  cliList = catMaybes $
       map flagToMaybe [cmd^.cmdAllow, cmd^.fallThrgh]
    <> [cmd^.iToken, cmd^.oToken, cmd^.cmpSeq, cmd^.initStr]

  -- | Convert command line flags to a 'Maybe' type, where the non-presence, as
  -- well as the default value of a flag will be interpreted as @Nothing@
  flagToMaybe :: DefSetting -> Maybe DefSetting
  flagToMaybe = \case
    SAllowCmd    b -> if b then Just (SAllowCmd    b) else Nothing
    SFallThrough b -> if b then Just (SFallThrough b) else Nothing
    _              -> Nothing

  -- | Insert all command line options, potentially overwriting already existing
  -- options that were given in the configuration file. This is a paramorphism
  insertCliOption :: DefSettings -> DefSettings -> DefSettings
  insertCliOption cliSettings cfgSettings =
    foldr (\s cfgs ->
             if   s `elem` cfgs
             then foldr (\x xs -> (if s == x then s else x) : xs) [] cfgs
             else s : cfgs)
          cfgSettings
          cliSettings