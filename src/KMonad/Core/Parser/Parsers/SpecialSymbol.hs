module KMonad.Core.Parser.Parsers.SpecialSymbol

where


import KMonad.Core.SpecialSymbol
import KMonad.Core.Parser.Parsers.KeySequence
import KMonad.Core.Parser.Utility

specialSymbols :: [SpecialSymbol]
specialSymbols =
  let
    mkSeq "" = Nothing
    mkSeq s  = Just $ parseE (concat <$> (many . lexeme $ seqElem)) s
    f (a, b, c) = SpecialSymbol a b (mkSeq c)
  in map f $
    [ -- Currency
      ("Euro sign"    , '€', "= c")
    , ("Pound sign"   , '₤', "= l")

      -- Accents
    , ("A grave"      , 'À', "` A")
    , ("a grave"      , 'à', "` a")
    , ("a circumflex" , 'â', "^ a")
    ]
