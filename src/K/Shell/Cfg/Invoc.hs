{-# LANGUAGE QuasiQuotes #-}
-- |

module K.Shell.Cfg.Invoc where

import K.Initial.Parsing hiding (option)
import K.Shell.Cfg.Initial

import K.Shell.Cfg.Cfgable
import K.Shell.Cfg.Default

-- import qualified KMonad.Prelude.Parsing as P (_ParseError, ParseError)
-- import KMonad.App.Cfg.Types hiding (Invoc, HasInvoc(..))
-- import KMonad.App.Cfg.Cfgable hiding (option, flag)
-- import KMonad.App.Cfg.Default

-- FIXME
-- Imports required to add versioner to command
-- import KMonad.Args.TH (gitHash)
-- import Paths_kmonad (version)
-- import Data.Version (showVersion)

import Options.Applicative hiding (Parser, flag, option)

import Text.RawString.QQ
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as L

import qualified Options.Applicative as O (Parser, flag, option)



newtype Invoc = Invoc { _cfgChange :: Change ShellCfg }
makeLenses ''Invoc

instance HasChange Invoc ShellCfg where change = cfgChange

-- help-text -------------------------------------------------------------------

-- TODO: Improve help-doc

(<^>) :: Doc -> Doc -> Doc
(<^>) = (L.<$>)

headDoc :: Doc
headDoc = [r|
Start running a KMonad process. See https://github.com/kmonad/kmonad.git for
more extensive documentation. How KMonad acquires and remaps events depends largely
on a dhall configuration file (by default: $XDG_CONFIG_HOME/kmonad/kmonad.dhall)
and a keymap (by default: $XDG_CONFIG_HOME/kmonad/keymap.kbd).
|]

specTxt :: Doc
specTxt = [r|
Many of the configuration options operate on a small DSL further documented on the
github page.
|]

-- running ---------------------------------------------------------------------

-- | Show help
ivkPrefs :: ParserPrefs
ivkPrefs = prefs mempty

ivkInfo :: ParserInfo Invoc
ivkInfo = info (invocP <**> versioner <**> helper)
          (  fullDesc
            <> progDescDoc (Just $ headDoc <^> specTxt)
            <> header   "kmonad - an onion of buttons."
          )

-- | Generate an invocation from Text
--
-- NOTE: This function is only to be used for testing purposes. It allows you to
-- test an invocation without running the command from the prompt.
testInvoc :: Text -> Invoc
testInvoc t = case execParserPure ivkPrefs ivkInfo . words . unpack $ t of
  Success a -> a
  _ -> devFail $ t <> " is not a valid kmonad invocation"

-- | Run a function that requires an 'Invoc' on the 'Invoc' from IO
withInvoc :: MonadIO m => Ctx r m Invoc -- (Invoc -> m a) -> m a
withInvoc = ContT $ \f -> f =<< liftIO (customExecParser ivkPrefs ivkInfo)

-- FIXME
-- | Equip a parser with version information about the program
versioner :: O.Parser (a -> a)
versioner = pure id
-- versioner = infoOption (showVersion version <> ", commit " <> $(gitHash))
--   (  long "version"
--   <> short 'V'
--   <> help "Show version"
--   )

-- parsers ---------------------------------------------------------------------

-- | Parse the entire invocation
invocP :: O.Parser Invoc
invocP = Invoc <$> appMods

-- | Construct flags and options from 'shellFlags' and 'shellOptions'
appMods :: O.Parser (Change ShellCfg)
appMods = let f = filter (hasn't $ source._FromCfgFile) shellFlags
              o = filter (hasn't $ source._FromCfgFile) shellOptions
          in fmap mconcat . sequenceA $ map flagP f <> map optionP o

-- | Turn 1 'ShellFlag' into an optparse-applicative 'flag'
flagP :: ShellFlag -> O.Parser (Change ShellCfg)
flagP f = O.flag mempty (f^.change) $ mconcat
  [ long $ unpack (f^.longName)
  , help $ unpack (f^.doc)
  ] <> maybe mempty short (f^.shortName)

-- | Turn 1 'ShellOption' into an optparse-applicative 'option'
optionP :: ShellOption -> O.Parser (Change ShellCfg)
optionP o = O.option (eitherReader f) $ mconcat
  [ long $ unpack (o^.longName)
  , help $ unpack (o^.doc)
  , value mempty
  ] <> maybe mempty short (o^.shortName)
  where
    f s = left show $ (o^.mkChange) (pack s)
