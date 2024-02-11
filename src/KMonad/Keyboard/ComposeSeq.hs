{-|
Module      : KMonad.Keyboard.ComposeSeq
Description : A list of compose-sequences
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

This module contains only a Haskellified list of (nearly) all X11 compose-key
sequences. For each entry we have the sequence of keys that defines it, the
UTF-8 character that it represents, and the X11 name for this
sequence/character.

-}
module KMonad.Keyboard.ComposeSeq
  ( -- * Sequences
    ssComposed
  )
where

import KMonad.Prelude

--------------------------------------------------------------------------------

-- | A collection of all supported compose-key sequences (nearly all X11
-- compose-key sequences). Each tuple consists of:
-- 1. A string that, when parsed to a tap-macro, will emit the sequence.
-- 2. The UTF-8 character that it represents
-- 3. A descriptive-name
--
ssComposed :: [(Text, Char, Text)]
ssComposed =
    [ ("' '"      , '´'     , "acute")
    , ( "^ -"     , '¯'     , "macron" )
    , ( "spc ("   , '˘'     , "breve" )
    , ( "\" \""   , '¨'     , "diaeresis" )
    , ("spc <"    , 'ˇ'     , "caron")
    , ("` spc"    , '`'     , "grave")
    , (", spc"    , '¸'     , "cedilla")
    , ("spc spc"  , ' '     , "nobreakspace")
    , ("spc ."    , ' '     , "U2008")
    , ("o c"      , '©'     , "copyright")
    , ("o r"      , '®'     , "registered")
    , (". >"      , '›'     , "U203a")
    , (". <"      , '‹'     , "U2039")
    , (". ."      , '…'     , "ellipsis")
    , (". -"      , '·'     , "periodcentered")
    , (". ="      , '•'     , "enfilledcircbullet")
    , ("! ^"      , '¦'     , "brokenbar")
    , ("! !"      , '¡'     , "exclamdown")
    , ("p !"      , '¶'     , "paragraph")
    , ("+ -"      , '±'     , "plusminus")
    , ("? ?"      , '¿'     , "questiondown")
    , ("s s"      , 'ß'     , "ssharp")
    , ("S S"      , 'ẞ'     , "U1e9e")
    , ("o e"      , 'œ'     , "oe")
    , ("O E"      , 'Œ'     , "OE")
    , ("a e"      , 'æ'     , "ae")
    , ("A E"      , 'Æ'     , "AE")
    , ("f f"      , 'ﬀ'     , "Ufb00")
    , ("f i"      , 'ﬁ'     , "Ufb01")
    , ("f l"      , 'ﬂ'     , "Ufb02")
    , ("F i"      , 'ﬃ'     , "Ufb03")
    , ("F l"      , 'ﬄ'     , "Ufb04")
    , ("I J"      , 'Ĳ'     , "U0132")
    , ("i j"      , 'ĳ'     , "U0133")
    , ("o o"      , '°'     , "degree")
    , ("< <"      , '«'     , "guillemotleft")
    , ("> >"      , '»'     , "guillemotright")
    , ("' <"      , '‘'     , "U2018")
    , ("> '"      , '’'     , "U2019")
    , (", '"      , '‚'     , "U201a")
    , ("\" <"     , '“'     , "U201c")
    , ("\" >"     , '”'     , "U201d")
    , ("\" ,"     , '„'     , "U201e")
    , ("% o"      , '‰'     , "U2030")
    , ("C E"      , '₠'     , "U20a0")
    , ("/ C"      , '₡'     , "U20a1")
    , ("C r"      , '₢'     , "U20a2")
    , ("F r"      , '₣'     , "U20a3")
    , ("= L"      , '₤'     , "U20a4")
    , ("/ m"      , '₥'     , "U20a5")
    , ("= N"      , '₦'     , "U20a6")
    , ("P t"      , '₧'     , "U20a7")
    , ("R s"      , '₨'     , "U20a8")
    , ("= W"      , '₩'     , "U20a9")
    , ("= d"      , '₫'     , "U20ab")
    , ("C ="      , '€'     , "EuroSign")
    , ("P ="      , '₽'     , "U20bd")
    , ("R ="      , '₹'     , "U20b9")
    , ("C |"      , '¢'     , "cent")
    , ("L -"      , '£'     , "sterling")
    , ("Y ="      , '¥'     , "yen")
    , ("f s"      , 'ſ'     , "U017f")
    , ("- - ."    , '–'     , "U2013")
    , ("- - -"    , '—'     , "U2014")
    , ("# q"      , '♩'     , "U2669")
    , ("# e"      , '♪'     , "U266a")
    , ("# E"      , '♫'     , "U266b")
    , ("# S"      , '♬'     , "U266c")
    , ("# b"      , '♭'     , "U266d")
    , ("# f"      , '♮'     , "U266e")
    , ("# #"      , '♯'     , "U266f")
    , ("s o"      , '§'     , "section")
    , ("o x"      , '¤'     , "currency")
    , ("N o"      , '№'     , "numerosign")
    , ("? !"      , '⸘'     , "U2E18")
    , ("! ?"      , '‽'     , "U203D")
    , ("C C C P"  , '☭'     , "U262D")
    , ("O A"      , 'Ⓐ'     , "U24B6")
    , ("< 3"      , '♥'     , "U2665")
    , (": )"      , '☺'     , "U263A")
    , (": ("      , '☹'     , "U2639")
    , ("\\ o /"   , '🙌'    , "Man with arms raised")
    , ("p o o"    , '💩'    , "U1F4A9")
    , ("L L A P"  , '🖖'    , "U1F596")
    , (", -"      , '¬'     , "notsign")
    , ("^ _ a"    , 'ª'     , "ordfeminine")
    , ("^ 2"      , '²'     , "twosuperior")
    , ("^ 3"      , '³'     , "threesuperior")
    , ("m u"      , 'µ'     , "mu")
    , ("^ 1"      , '¹'     , "onesuperior")
    , ("^ _ o"    , 'º'     , "masculine")
    , ("1 4"      , '¼'     , "onequarter")
    , ("1 2"      , '½'     , "onehalf")
    , ("3 4"      , '¾'     , "threequarters")
    , ("A `"      , 'À'     , "Agrave")
    , ("' A"      , 'Á'     , "Aacute")
    , ("^ A"      , 'Â'     , "Acircumflex")
    , ("~ A"      , 'Ã'     , "Atilde")
    , ("\" A"     , 'Ä'     , "Adiaeresis")
    , ("o A"      , 'Å'     , "Aring")
    , (", C"      , 'Ç'     , "Ccedilla")
    , ("` E"      , 'È'     , "Egrave")
    , ("' E"      , 'É'     , "Eacute")
    , ("^ E"      , 'Ê'     , "Ecircumflex")
    , ("^ U"      , 'Û'     , "Ucircumflex")
    , ("` U"      , 'Ù'     , "Ugrave")
    , ("\" E"     , 'Ë'     , "Ediaeresis")
    , ("` I"      , 'Ì'     , "Igrave")
    , ("' I"      , 'Í'     , "Iacute")
    , ("I ^"      , 'Î'     , "Icircumflex")
    , ("\" I"     , 'Ï'     , "Idiaeresis")
    , ("D H"      , 'Ð'     , "ETH")
    , ("~ N"      , 'Ñ'     , "Ntilde")
    , ("` O"      , 'Ò'     , "Ograve")
    , ("' O"      , 'Ó'     , "Oacute")
    , ("^ O"      , 'Ô'     , "Ocircumflex")
    , ("~ O"      , 'Õ'     , "Otilde")
    , ("\" O"     , 'Ö'     , "Odiaeresis")
    , ("\" U"     , 'Ü'     , "Udiaeresis")
    , ("' U"      , 'Ú'     , "Uacute")
    , ("x x"      , '×'     , "multiply")
    , ("/ O"      , 'Ø'     , "Oslash")
    , ("' Y"      , 'Ý'     , "Yacute")
    , ("T H"      , 'Þ'     , "THORN")
    , ("` a"      , 'à'     , "agrave")
    , ("' a"      , 'á'     , "aacute")
    , ("^ a"      , 'â'     , "acircumflex")
    , ("~ a"      , 'ã'     , "atilde")
    , ("\" a"     , 'ä'     , "adiaeresis")
    , ("o a"      , 'å'     , "aring")
    , (", c"      , 'ç'     , "ccedilla")
    , ("` e"      , 'è'     , "egrave")
    , ("' e"      , 'é'     , "eacute")
    , ("^ e"      , 'ê'     , "ecircumflex")
    , ("\" e"     , 'ë'     , "ediaeresis")
    , ("` i"      , 'ì'     , "igrave")
    , ("' i"      , 'í'     , "iacute")
    , ("^ i"      , 'î'     , "icircumflex")
    , ("\" i"     , 'ï'     , "idiaeresis")
    , ("d h"      , 'ð'     , "eth")
    , ("~ n"      , 'ñ'     , "ntilde")
    , ("` o"      , 'ò'     , "ograve")
    , ("' o"      , 'ó'     , "oacute")
    , ("^ o"      , 'ô'     , "ocircumflex")
    , ("~ o"      , 'õ'     , "otilde")
    , ("\" o"     , 'ö'     , "odiaeresis")
    , (": -"      , '÷'     , "division")
    , ("/ o"      , 'ø'     , "oslash")
    , ("` u"      , 'ù'     , "ugrave")
    , ("' u"      , 'ú'     , "uacute")
    , ("^ u"      , 'û'     , "ucircumflex")
    , ("\" u"     , 'ü'     , "udiaeresis")
    , ("' y"      , 'ý'     , "yacute")
    , ("t h"      , 'þ'     , "thorn")
    , ("\" y"     , 'ÿ'     , "ydiaeresis")
    , ("_ A"      , 'Ā'     , "U0100")
    , ("_ a"      , 'ā'     , "U0101")
    , ("u A"      , 'Ă'     , "U0102")
    , ("u a"      , 'ă'     , "U0103")
    , ("; A"      , 'Ą'     , "U0104")
    , ("; a"      , 'ą'     , "U0105")
    , ("' C"      , 'Ć'     , "U0106")
    , ("' c"      , 'ć'     , "U0107")
    , ("^ C"      , 'Ĉ'     , "U0108")
    , ("^ c"      , 'ĉ'     , "U0109")
    , ("C ."      , 'Ċ'     , "U010A")
    , (". c"      , 'ċ'     , "U010B")
    , ("c C"      , 'Č'     , "U010C")
    , ("c c"      , 'č'     , "U010D")
    , ("c D"      , 'Ď'     , "U010E")
    , ("c d"      , 'ď'     , "U010F")
    , ("- D"      , 'Đ'     , "Dstroke")
    , ("- d"      , 'đ'     , "dstroke")
    , ("_ E"      , 'Ē'     , "U0112")
    , ("_ e"      , 'ē'     , "U0113")
    , ("b E"      , 'Ĕ'     , "U0114")
    , ("b e"      , 'ĕ'     , "U0115")
    , (". E"      , 'Ė'     , "U0116")
    , (". e"      , 'ė'     , "U0117")
    , ("; E"      , 'Ę'     , "U0118")
    , ("; e"      , 'ę'     , "U0119")
    , ("c E"      , 'Ě'     , "U011A")
    , ("c e"      , 'ě'     , "U011B")
    , ("^ G"      , 'Ĝ'     , "U011C")
    , ("^ g"      , 'ĝ'     , "U011D")
    , ("b G"      , 'Ğ'     , "U011E")
    , ("b g"      , 'ğ'     , "U011F")
    , (". G"      , 'Ġ'     , "U0120")
    , (". g"      , 'ġ'     , "U0121")
    , ("G ,"      , 'Ģ'     , "U0122")
    , (", g"      , 'ģ'     , "U0123")
    , ("^ H"      , 'Ĥ'     , "U0124")
    , ("^ h"      , 'ĥ'     , "U0125")
    , ("/ H"      , 'Ħ'     , "U0126")
    , ("/ h"      , 'ħ'     , "U0127")
    , ("~ I"      , 'Ĩ'     , "U0128")
    , ("~ i"      , 'ĩ'     , "U0129")
    , ("_ I"      , 'Ī'     , "U012A")
    , ("_ i"      , 'ī'     , "U012B")
    , ("b I"      , 'Ĭ'     , "U012C")
    , ("b i"      , 'ĭ'     , "U012D")
    , ("; I"      , 'Į'     , "U012E")
    , ("; i"      , 'į'     , "U012F")
    , (". I"      , 'İ'     , "U0130")
    , ("i ."      , 'ı'     , "U0131")
    , ("^ J"      , 'Ĵ'     , "U0134")
    , ("^ j"      , 'ĵ'     , "U0135")
    , (", K"      , 'Ķ'     , "U0136")
    , (", k"      , 'ķ'     , "U0137")
    , ("k k"      , 'ĸ'     , "U0138")
    , ("' L"      , 'Ĺ'     , "U0139")
    , ("' l"      , 'ĺ'     , "U013A")
    , (", L"      , 'Ļ'     , "U013B")
    , (", l"      , 'ļ'     , "U013C")
    , ("c L"      , 'Ľ'     , "U013D")
    , ("c l"      , 'ľ'     , "U013E")
    , ("/ L"      , 'Ł'     , "U0141")
    , ("/ l"      , 'ł'     , "U0142")
    , ("' N"      , 'Ń'     , "U0143")
    , ("' n"      , 'ń'     , "U0144")
    , (", N"      , 'Ņ'     , "U0145")
    , (", n"      , 'ņ'     , "U0146")
    , ("c N"      , 'Ň'     , "U0147")
    , ("c n"      , 'ň'     , "U0148")
    , ("N G"      , 'Ŋ'     , "U014A")
    , ("n g"      , 'ŋ'     , "U014B")
    , ("_ O"      , 'Ō'     , "U014C")
    , ("_ o"      , 'ō'     , "U014D")
    , ("b O"      , 'Ŏ'     , "U014E")
    , ("b o"      , 'ŏ'     , "U014F")
    , ("= O"      , 'Ő'     , "U0150")
    , ("= o"      , 'ő'     , "U0151")
    , ("' R"      , 'Ŕ'     , "U0154")
    , ("' r"      , 'ŕ'     , "U0155")
    , ("R ,"      , 'Ŗ'     , "U0156")
    , (", r"      , 'ŗ'     , "U0157")
    , ("c R"      , 'Ř'     , "U0158")
    , ("c r"      , 'ř'     , "U0159")
    , ("' S"      , 'Ś'     , "U015A")
    , ("' s"      , 'ś'     , "U015B")
    , ("^ S"      , 'Ŝ'     , "U015C")
    , ("^ s"      , 'ŝ'     , "U015D")
    , (", S"      , 'Ş'     , "U015E")
    , (", s"      , 'ş'     , "U015F")
    , ("c S"      , 'Š'     , "U0160")
    , ("c s"      , 'š'     , "U0161")
    , (", T"      , 'Ţ'     , "U0162")
    , (", t"      , 'ţ'     , "U0163")
    , ("c T"      , 'Ť'     , "U0164")
    , ("c t"      , 'ť'     , "U0165")
    , ("/ T"      , 'Ŧ'     , "U0166")
    , ("/ t"      , 'ŧ'     , "U0167")
    , ("~ u"      , 'ũ'     , "U0169")
    , ("_ u"      , 'ū'     , "U016B")
    , ("u u"      , 'ŭ'     , "U016D")
    , ("o u"      , 'ů'     , "U016F")
    , ("= u"      , 'ű'     , "U0171")
    , ("; u"      , 'ų'     , "U0173")
    , ("^ W"      , 'Ŵ'     , "U0174")
    , ("^ w"      , 'ŵ'     , "U0175")
    , ("^ Y"      , 'Ŷ'     , "U0176")
    , ("^ y"      , 'ŷ'     , "U0177")
    , ("\" Y"     , 'Ÿ'     , "U0178")
    , ("' Z"      , 'Ź'     , "U0179")
    , ("' z"      , 'ź'     , "U017A")
    , (". Z"      , 'Ż'     , "U017B")
    , (". z"      , 'ż'     , "U017C")
    , ("c Z"      , 'Ž'     , "U017D")
    , ("c z"      , 'ž'     , "U017E")
    , ("/ b"      , 'ƀ'     , "U0180")
    , ("/ I"      , 'Ɨ'     , "U0197")
    , ("+ O"      , 'Ơ'     , "U01A0")
    , ("+ o"      , 'ơ'     , "U01A1")
    , ("+ u"      , 'ư'     , "U01B0")
    , ("/ Z"      , 'Ƶ'     , "U01B5")
    , ("/ z"      , 'ƶ'     , "U01B6")
    , ("c A"      , 'Ǎ'     , "U01CD")
    , ("c a"      , 'ǎ'     , "U01CE")
    , ("c I"      , 'Ǐ'     , "U01CF")
    , ("c i"      , 'ǐ'     , "U01D0")
    , ("c O"      , 'Ǒ'     , "U01D1")
    , ("c o"      , 'ǒ'     , "U01D2")
    , ("c u"      , 'ǔ'     , "U01D4")
    , ("_ \" u"   , 'ǖ'     , "U01D6")
    , ("' \" u"   , 'ǘ'     , "U01D8")
    , ("c \" u"   , 'ǚ'     , "U01DA")
    , ("` \" u"   , 'ǜ'     , "U01DC")
    , ("_ \" A"   , 'Ǟ'     , "U01DE")
    , ("_ \" a"   , 'ǟ'     , "U01DF")
    , ("_ . A"    , 'Ǡ'     , "U01E0")
    , ("_ . a"    , 'ǡ'     , "U01E1")
    , ("/ G"      , 'Ǥ'     , "U01E4")
    , ("/ g"      , 'ǥ'     , "U01E5")
    , ("c G"      , 'Ǧ'     , "U01E6")
    , ("c g"      , 'ǧ'     , "U01E7")
    , ("c K"      , 'Ǩ'     , "U01E8")
    , ("c k"      , 'ǩ'     , "U01E9")
    , ("; O"      , 'Ǫ'     , "U01EA")
    , ("; o"      , 'ǫ'     , "U01EB")
    , ("_ ; O"    , 'Ǭ'     , "U01EC")
    , ("_ ; o"    , 'ǭ'     , "U01ED")
    , ("c j"      , 'ǰ'     , "U01F0")
    , ("' G"      , 'Ǵ'     , "U01F4")
    , ("' g"      , 'ǵ'     , "U01F5")
    , ("` N"      , 'Ǹ'     , "U01F8")
    , ("` n"      , 'ǹ'     , "U01F9")
    , ("* ' A"    , 'Ǻ'     , "U01FA")
    , ("* ' a"    , 'ǻ'     , "U01FB")
    , ("' / O"    , 'Ǿ'     , "U01FE")
    , ("c H"      , 'Ȟ'     , "U021E")
    , ("c h"      , 'ȟ'     , "U021F")
    , (". A"      , 'Ȧ'     , "U0226")
    , (". a"      , 'ȧ'     , "U0227")
    , ("_ \" O"   , 'Ȫ'     , "U022A")
    , ("_ \" o"   , 'ȫ'     , "U022B")
    , ("_ ~ O"    , 'Ȭ'     , "U022C")
    , ("_ ~ o"    , 'ȭ'     , "U022D")
    , (". O"      , 'Ȯ'     , "U022E")
    , (". o"      , 'ȯ'     , "U022F")
    , ("_ . O"    , 'Ȱ'     , "U0230")
    , ("_ . o"    , 'ȱ'     , "U0231")
    , ("_ Y"      , 'Ȳ'     , "U0232")
    , ("_ y"      , 'ȳ'     , "U0233")
    , ("e e"      , 'ə'     , "U0259")
    , ("/ i"      , 'ɨ'     , "U0268")
    , ("^ _ h"    , 'ʰ'     , "U02B0")
    , ("^ _ j"    , 'ʲ'     , "U02B2")
    , ("^ _ r"    , 'ʳ'     , "U02B3")
    , ("^ _ w"    , 'ʷ'     , "U02B7")
    , ("^ _ y"    , 'ʸ'     , "U02B8")
    , ("^ _ l"    , 'ˡ'     , "U02E1")
    , ("^ _ s"    , 'ˢ'     , "U02E2")
    , ("^ _ x"    , 'ˣ'     , "U02E3")
    , ("\" '"     , '̈́'      , "U0344")
    , ("' \" spc" , '΅'     , "U0385")
    , (". B"      , 'Ḃ'     , "U1E02")
    , (". b"      , 'ḃ'     , "U1E03")
    , ("! B"      , 'Ḅ'     , "U1E04")
    , ("! b"      , 'ḅ'     , "U1E05")
    , (". D"      , 'Ḋ'     , "U1E0A")
    , (". d"      , 'ḋ'     , "U1E0B")
    , ("! D"      , 'Ḍ'     , "U1E0C")
    , ("! d"      , 'ḍ'     , "U1E0D")
    , (", D"      , 'Ḑ'     , "U1E10")
    , (", d"      , 'ḑ'     , "U1E11")
    , ("` _ E"    , 'Ḕ'     , "U1E14")
    , ("` _ e"    , 'ḕ'     , "U1E15")
    , ("' _ E"    , 'Ḗ'     , "U1E16")
    , ("' _ e"    , 'ḗ'     , "U1E17")
    , ("b , E"    , 'Ḝ'     , "U1E1C")
    , ("b , e"    , 'ḝ'     , "U1E1D")
    , (". F"      , 'Ḟ'     , "U1E1E")
    , ("f ."      , 'ḟ'     , "U1E1F")
    , ("_ G"      , 'Ḡ'     , "U1E20")
    , ("_ g"      , 'ḡ'     , "U1E21")
    , (". H"      , 'Ḣ'     , "U1E22")
    , (". h"      , 'ḣ'     , "U1E23")
    , ("! H"      , 'Ḥ'     , "U1E24")
    , ("! h"      , 'ḥ'     , "U1E25")
    , ("\" H"     , 'Ḧ'     , "U1E26")
    , ("\" h"     , 'ḧ'     , "U1E27")
    , (", H"      , 'Ḩ'     , "U1E28")
    , (", h"      , 'ḩ'     , "U1E29")
    , ("' \" I"   , 'Ḯ'     , "U1E2E")
    , ("' \" i"   , 'ḯ'     , "U1E2F")
    , ("' K"      , 'Ḱ'     , "U1E30")
    , ("' k"      , 'ḱ'     , "U1E31")
    , ("! K"      , 'Ḳ'     , "U1E32")
    , ("! k"      , 'ḳ'     , "U1E33")
    , ("! L"      , 'Ḷ'     , "U1E36")
    , ("! l"      , 'ḷ'     , "U1E37")
    , ("_ ! L"    , 'Ḹ'     , "U1E38")
    , ("_ ! l"    , 'ḹ'     , "U1E39")
    , ("' M"      , 'Ḿ'     , "U1E3E")
    , ("' m"      , 'ḿ'     , "U1E3F")
    , (". M"      , 'Ṁ'     , "U1E40")
    , (". m"      , 'ṁ'     , "U1E41")
    , ("! M"      , 'Ṃ'     , "U1E42")
    , ("! m"      , 'ṃ'     , "U1E43")
    , (". N"      , 'Ṅ'     , "U1E44")
    , (". n"      , 'ṅ'     , "U1E45")
    , ("! N"      , 'Ṇ'     , "U1E46")
    , ("! n"      , 'ṇ'     , "U1E47")
    , ("' ~ O"    , 'Ṍ'     , "U1E4C")
    , ("' ~ o"    , 'ṍ'     , "U1E4D")
    , ("\" ~ O"   , 'Ṏ'     , "U1E4E")
    , ("\" ~ o"   , 'ṏ'     , "U1E4F")
    , ("` _ O"    , 'Ṑ'     , "U1E50")
    , ("` _ o"    , 'ṑ'     , "U1E51")
    , ("' _ O"    , 'Ṓ'     , "U1E52")
    , ("' _ o"    , 'ṓ'     , "U1E53")
    , ("' P"      , 'Ṕ'     , "U1E54")
    , ("' p"      , 'ṕ'     , "U1E55")
    , (". P"      , 'Ṗ'     , "U1E56")
    , (". p"      , 'ṗ'     , "U1E57")
    , (". R"      , 'Ṙ'     , "U1E58")
    , (". r"      , 'ṙ'     , "U1E59")
    , ("! R"      , 'Ṛ'     , "U1E5A")
    , ("! r"      , 'ṛ'     , "U1E5B")
    , ("_ ! R"    , 'Ṝ'     , "U1E5C")
    , ("_ ! r"    , 'ṝ'     , "U1E5D")
    , (". S"      , 'Ṡ'     , "U1E60")
    , (". s"      , 'ṡ'     , "U1E61")
    , ("! S"      , 'Ṣ'     , "U1E62")
    , ("! s"      , 'ṣ'     , "U1E63")
    , (". ' S"    , 'Ṥ'     , "U1E64")
    , (". ' s"    , 'ṥ'     , "U1E65")
    , (". ! S"    , 'Ṩ'     , "U1E68")
    , (". ! s"    , 'ṩ'     , "U1E69")
    , (". T"      , 'Ṫ'     , "U1E6A")
    , (". t"      , 'ṫ'     , "U1E6B")
    , ("! T"      , 'Ṭ'     , "U1E6C")
    , ("! t"      , 'ṭ'     , "U1E6D")
    , ("' ~ u"    , 'ṹ'     , "U1E79")
    , ("\" _ u"   , 'ṻ'     , "U1E7B")
    , ("~ V"      , 'Ṽ'     , "U1E7C")
    , ("~ v"      , 'ṽ'     , "U1E7D")
    , ("! V"      , 'Ṿ'     , "U1E7E")
    , ("! v"      , 'ṿ'     , "U1E7F")
    , ("` W"      , 'Ẁ'     , "U1E80")
    , ("` w"      , 'ẁ'     , "U1E81")
    , ("' W"      , 'Ẃ'     , "U1E82")
    , ("' w"      , 'ẃ'     , "U1E83")
    , ("\" W"     , 'Ẅ'     , "U1E84")
    , ("\" w"     , 'ẅ'     , "U1E85")
    , (". W"      , 'Ẇ'     , "U1E86")
    , (". w"      , 'ẇ'     , "U1E87")
    , ("! W"      , 'Ẉ'     , "U1E88")
    , ("! w"      , 'ẉ'     , "U1E89")
    , (". X"      , 'Ẋ'     , "U1E8A")
    , (". x"      , 'ẋ'     , "U1E8B")
    , ("\" X"     , 'Ẍ'     , "U1E8C")
    , ("\" x"     , 'ẍ'     , "U1E8D")
    , (". Y"      , 'Ẏ'     , "U1E8E")
    , (". y"      , 'ẏ'     , "U1E8F")
    , ("^ Z"      , 'Ẑ'     , "U1E90")
    , ("^ z"      , 'ẑ'     , "U1E91")
    , ("! Z"      , 'Ẓ'     , "U1E92")
    , ("! z"      , 'ẓ'     , "U1E93")
    , ("\" t"     , 'ẗ'     , "U1E97")
    , ("o w"      , 'ẘ'     , "U1E98")
    , ("o y"      , 'ẙ'     , "U1E99")
    , ("! A"      , 'Ạ'     , "U1EA0")
    , ("! a"      , 'ạ'     , "U1EA1")
    , ("? A"      , 'Ả'     , "U1EA2")
    , ("? a"      , 'ả'     , "U1EA3")
    , ("' ^ A"    , 'Ấ'     , "U1EA4")
    , ("' ^ a"    , 'ấ'     , "U1EA5")
    , ("` ^ A"    , 'Ầ'     , "U1EA6")
    , ("` ^ a"    , 'ầ'     , "U1EA7")
    , ("? ^ A"    , 'Ẩ'     , "U1EA8")
    , ("? ^ a"    , 'ẩ'     , "U1EA9")
    , ("~ ^ A"    , 'Ẫ'     , "U1EAA")
    , ("~ ^ a"    , 'ẫ'     , "U1EAB")
    , ("^ ! A"    , 'Ậ'     , "U1EAC")
    , ("^ ! a"    , 'ậ'     , "U1EAD")
    , ("' b A"    , 'Ắ'     , "U1EAE")
    , ("' b a"    , 'ắ'     , "U1EAF")
    , ("` b A"    , 'Ằ'     , "U1EB0")
    , ("` b a"    , 'ằ'     , "U1EB1")
    , ("? b A"    , 'Ẳ'     , "U1EB2")
    , ("? b a"    , 'ẳ'     , "U1EB3")
    , ("~ b A"    , 'Ẵ'     , "U1EB4")
    , ("~ b a"    , 'ẵ'     , "U1EB5")
    , ("b ! A"    , 'Ặ'     , "U1EB6")
    , ("b ! a"    , 'ặ'     , "U1EB7")
    , ("! E"      , 'Ẹ'     , "U1EB8")
    , ("! e"      , 'ẹ'     , "U1EB9")
    , ("? E"      , 'Ẻ'     , "U1EBA")
    , ("? e"      , 'ẻ'     , "U1EBB")
    , ("~ E"      , 'Ẽ'     , "U1EBC")
    , ("~ e"      , 'ẽ'     , "U1EBD")
    , ("' ^ E"    , 'Ế'     , "U1EBE")
    , ("' ^ e"    , 'ế'     , "U1EBF")
    , ("` ^ E"    , 'Ề'     , "U1EC0")
    , ("` ^ e"    , 'ề'     , "U1EC1")
    , ("? ^ E"    , 'Ể'     , "U1EC2")
    , ("? ^ e"    , 'ể'     , "U1EC3")
    , ("~ ^ E"    , 'Ễ'     , "U1EC4")
    , ("~ ^ e"    , 'ễ'     , "U1EC5")
    , ("^ ! E"    , 'Ệ'     , "U1EC6")
    , ("^ ! e"    , 'ệ'     , "U1EC7")
    , ("? I"      , 'Ỉ'     , "U1EC8")
    , ("? i"      , 'ỉ'     , "U1EC9")
    , ("! I"      , 'Ị'     , "U1ECA")
    , ("! i"      , 'ị'     , "U1ECB")
    , ("! O"      , 'Ọ'     , "U1ECC")
    , ("! o"      , 'ọ'     , "U1ECD")
    , ("? O"      , 'Ỏ'     , "U1ECE")
    , ("? o"      , 'ỏ'     , "U1ECF")
    , ("' ^ O"    , 'Ố'     , "U1ED0")
    , ("' ^ o"    , 'ố'     , "U1ED1")
    , ("` ^ O"    , 'Ồ'     , "U1ED2")
    , ("` ^ o"    , 'ồ'     , "U1ED3")
    , ("? ^ O"    , 'Ổ'     , "U1ED4")
    , ("? ^ o"    , 'ổ'     , "U1ED5")
    , ("~ ^ O"    , 'Ỗ'     , "U1ED6")
    , ("~ ^ o"    , 'ỗ'     , "U1ED7")
    , ("^ ! O"    , 'Ộ'     , "U1ED8")
    , ("^ ! o"    , 'ộ'     , "U1ED9")
    , ("' + O"    , 'Ớ'     , "U1EDA")
    , ("' + o"    , 'ớ'     , "U1EDB")
    , ("` + O"    , 'Ờ'     , "U1EDC")
    , ("` + o"    , 'ờ'     , "U1EDD")
    , ("? + O"    , 'Ở'     , "U1EDE")
    , ("? + o"    , 'ở'     , "U1EDF")
    , ("~ + O"    , 'Ỡ'     , "U1EE0")
    , ("~ + o"    , 'ỡ'     , "U1EE1")
    , ("! + O"    , 'Ợ'     , "U1EE2")
    , ("! + o"    , 'ợ'     , "U1EE3")
    , ("! u"      , 'ụ'     , "U1EE5")
    , ("? u"      , 'ủ'     , "U1EE7")
    , ("' + u"    , 'ứ'     , "U1EE9")
    , ("` + u"    , 'ừ'     , "U1EEB")
    , ("? + u"    , 'ử'     , "U1EED")
    , ("~ + u"    , 'ữ'     , "U1EEF")
    , ("! + u"    , 'ự'     , "U1EF1")
    , ("` Y"      , 'Ỳ'     , "U1EF2")
    , ("` y"      , 'ỳ'     , "U1EF3")
    , ("! Y"      , 'Ỵ'     , "U1EF4")
    , ("! y"      , 'ỵ'     , "U1EF5")
    , ("? Y"      , 'Ỷ'     , "U1EF6")
    , ("? y"      , 'ỷ'     , "U1EF7")
    , ("~ Y"      , 'Ỹ'     , "U1EF8")
    , ("~ y"      , 'ỹ'     , "U1EF9")
    , ("^ 0"      , '⁰'     , "U2070")
    , ("^ _ i"    , 'ⁱ'     , "U2071")
    , ("^ 4"      , '⁴'     , "U2074")
    , ("^ 5"      , '⁵'     , "U2075")
    , ("^ 6"      , '⁶'     , "U2076")
    , ("^ 7"      , '⁷'     , "U2077")
    , ("^ 8"      , '⁸'     , "U2078")
    , ("^ 9"      , '⁹'     , "U2079")
    , ("^ +"      , '⁺'     , "U207A")
    , ("^ ="      , '⁼'     , "U207C")
    , ("^ ("      , '⁽'     , "U207D")
    , ("^ )"      , '⁾'     , "U207E")
    , ("^ _ n"    , 'ⁿ'     , "U207F")
    , ("_ 0"      , '₀'     , "U2080")
    , ("_ 1"      , '₁'     , "U2081")
    , ("_ 2"      , '₂'     , "U2082")
    , ("_ 3"      , '₃'     , "U2083")
    , ("_ 4"      , '₄'     , "U2084")
    , ("_ 5"      , '₅'     , "U2085")
    , ("_ 6"      , '₆'     , "U2086")
    , ("_ 7"      , '₇'     , "U2087")
    , ("_ 8"      , '₈'     , "U2088")
    , ("_ 9"      , '₉'     , "U2089")
    , ("_ +"      , '₊'     , "U208A")
    , ("_ ="      , '₌'     , "U208C")
    , ("_ ("      , '₍'     , "U208D")
    , ("_ )"      , '₎'     , "U208E")
    , ("S M"      , '℠'     , "U2120")
    , ("T M"      , '™'     , "U2122")
    , ("1 7"      , '⅐'     , "U2150")
    , ("1 9"      , '⅑'     , "U2151")
    , ("1 1 0"    , '⅒'     , "U2152")
    , ("1 3"      , '⅓'     , "U2153")
    , ("2 3"      , '⅔'     , "U2154")
    , ("1 5"      , '⅕'     , "U2155")
    , ("2 5"      , '⅖'     , "U2156")
    , ("3 5"      , '⅗'     , "U2157")
    , ("4 5"      , '⅘'     , "U2158")
    , ("1 6"      , '⅙'     , "U2159")
    , ("5 6"      , '⅚'     , "U215A")
    , ("1 8"      , '⅛'     , "U215B")
    , ("3 8"      , '⅜'     , "U215C")
    , ("5 8"      , '⅝'     , "U215D")
    , ("7 8"      , '⅞'     , "U215E")
    , ("0 3"      , '↉'     , "U2189")
    , ("/ lft"    , '↚'     , "U219A")
    , ("/ rght"   , '↛'     , "U219B")
    , ("< -"      , '←'     , "U2190")
    , ("- >"      , '→'     , "U2192")
    , ("= >"      , '⇒'     , "U21D2")
    , ("{ }"      , '∅'     , "U2205")
    , ("/ ="      , '≠'     , "U2260")
    , ("d i"      , '⌀'     , "U2300")
    , ("( 1 )"    , '①'     , "U2460")
    , ("( 2 )"    , '②'     , "U2461")
    , ("( 3 )"    , '③'     , "U2462")
    , ("( 4 )"    , '④'     , "U2463")
    , ("( 5 )"    , '⑤'     , "U2464")
    , ("( 6 )"    , '⑥'     , "U2465")
    , ("( 7 )"    , '⑦'     , "U2466")
    , ("( 8 )"    , '⑧'     , "U2467")
    , ("( 9 )"    , '⑨'     , "U2468")
    , ("( 1 0 )"  , '⑩'     , "U2469")
    , ("( 1 1 )"  , '⑪'     , "U246A")
    , ("( 1 2 )"  , '⑫'     , "U246B")
    , ("( 1 3 )"  , '⑬'     , "U246C")
    , ("( 1 4 )"  , '⑭'     , "U246D")
    , ("( 1 5 )"  , '⑮'     , "U246E")
    , ("( 1 6 )"  , '⑯'     , "U246F")
    , ("( 1 7 )"  , '⑰'     , "U2470")
    , ("( 1 8 )"  , '⑱'     , "U2471")
    , ("( 1 9 )"  , '⑲'     , "U2472")
    , ("( 2 0 )"  , '⑳'     , "U2473")
    , ("( B )"    , 'Ⓑ'     , "U24B7")
    , ("( C )"    , 'Ⓒ'     , "U24B8")
    , ("( D )"    , 'Ⓓ'     , "U24B9")
    , ("( E )"    , 'Ⓔ'     , "U24BA")
    , ("( F )"    , 'Ⓕ'     , "U24BB")
    , ("( G )"    , 'Ⓖ'     , "U24BC")
    , ("( H )"    , 'Ⓗ'     , "U24BD")
    , ("( I )"    , 'Ⓘ'     , "U24BE")
    , ("( J )"    , 'Ⓙ'     , "U24BF")
    , ("( K )"    , 'Ⓚ'     , "U24C0")
    , ("( L )"    , 'Ⓛ'     , "U24C1")
    , ("( M )"    , 'Ⓜ'     , "U24C2")
    , ("( N )"    , 'Ⓝ'     , "U24C3")
    , ("( O )"    , 'Ⓞ'     , "U24C4")
    , ("( P )"    , 'Ⓟ'     , "U24C5")
    , ("( Q )"    , 'Ⓠ'     , "U24C6")
    , ("( R )"    , 'Ⓡ'     , "U24C7")
    , ("( S )"    , 'Ⓢ'     , "U24C8")
    , ("( T )"    , 'Ⓣ'     , "U24C9")
    , ("( V )"    , 'Ⓥ'     , "U24CB")
    , ("( W )"    , 'Ⓦ'     , "U24CC")
    , ("( X )"    , 'Ⓧ'     , "U24CD")
    , ("( Y )"    , 'Ⓨ'     , "U24CE")
    , ("( Z )"    , 'Ⓩ'     , "U24CF")
    , ("( a )"    , 'ⓐ'     , "U24D0")
    , ("( b )"    , 'ⓑ'     , "U24D1")
    , ("( c )"    , 'ⓒ'     , "U24D2")
    , ("( d )"    , 'ⓓ'     , "U24D3")
    , ("( e )"    , 'ⓔ'     , "U24D4")
    , ("( f )"    , 'ⓕ'     , "U24D5")
    , ("( g )"    , 'ⓖ'     , "U24D6")
    , ("( h )"    , 'ⓗ'     , "U24D7")
    , ("( i )"    , 'ⓘ'     , "U24D8")
    , ("( j )"    , 'ⓙ'     , "U24D9")
    , ("( k )"    , 'ⓚ'     , "U24DA")
    , ("( l )"    , 'ⓛ'     , "U24DB")
    , ("( m )"    , 'ⓜ'     , "U24DC")
    , ("( n )"    , 'ⓝ'     , "U24DD")
    , ("( o )"    , 'ⓞ'     , "U24DE")
    , ("( p )"    , 'ⓟ'     , "U24DF")
    , ("( q )"    , 'ⓠ'     , "U24E0")
    , ("( r )"    , 'ⓡ'     , "U24E1")
    , ("( s )"    , 'ⓢ'     , "U24E2")
    , ("( t )"    , 'ⓣ'     , "U24E3")
    , ("( u )"    , 'ⓤ'     , "U24E4")
    , ("( v )"    , 'ⓥ'     , "U24E5")
    , ("( w )"    , 'ⓦ'     , "U24E6")
    , ("( x )"    , 'ⓧ'     , "U24E7")
    , ("( y )"    , 'ⓨ'     , "U24E8")
    , ("( z )"    , 'ⓩ'     , "U24E9")
    , ("( 0 )"    , '⓪'     , "U24EA")
    , ("( 2 1 )"  , '㉑'    , "U3251")
    , ("( 2 2 )"  , '㉒'    , "U3252")
    , ("( 2 3 )"  , '㉓'    , "U3253")
    , ("( 2 4 )"  , '㉔'    , "U3254")
    , ("( 2 5 )"  , '㉕'    , "U3255")
    , ("( 2 6 )"  , '㉖'    , "U3256")
    , ("( 2 7 )"  , '㉗'    , "U3257")
    , ("( 2 8 )"  , '㉘'    , "U3258")
    , ("( 2 9 )"  , '㉙'    , "U3259")
    , ("( 3 0 )"  , '㉚'    , "U325A")
    , ("( 3 1 )"  , '㉛'    , "U325B")
    , ("( 3 2 )"  , '㉜'    , "U325C")
    , ("( 3 3 )"  , '㉝'    , "U325D")
    , ("( 3 4 )"  , '㉞'    , "U325E")
    , ("( 3 5 )"  , '㉟'    , "U325F")
    , ("( 3 6 )"  , '㊱'    , "U32B1")
    , ("( 3 7 )"  , '㊲'    , "U32B2")
    , ("( 3 8 )"  , '㊳'    , "U32B3")
    , ("( 3 9 )"  , '㊴'    , "U32B4")
    , ("( 4 0 )"  , '㊵'    , "U32B5")
    , ("( 4 1 )"  , '㊶'    , "U32B6")
    , ("( 4 2 )"  , '㊷'    , "U32B7")
    , ("( 4 3 )"  , '㊸'    , "U32B8")
    , ("( 4 4 )"  , '㊹'    , "U32B9")
    , ("( 4 5 )"  , '㊺'    , "U32BA")
    , ("( 4 6 )"  , '㊻'    , "U32BB")
    , ("( 4 7 )"  , '㊼'    , "U32BC")
    , ("( 4 8 )"  , '㊽'    , "U32BD")
    , ("( 4 9 )"  , '㊾'    , "U32BE")
    , ("( 5 0 )"  , '㊿'    , "U32BF")
    , ("; S"      , 'Ș'     , "U0218")
    , ("; s"      , 'ș'     , "U0219")
    , ("; T"      , 'Ț'     , "U021A")
    , ("; t"      , 'ț'     , "U021B")
    , ("v /"      , '√'     , "U221a")
    , ("8 8"      , '∞'     , "U221e")
    , ("= _"      , '≡'     , "U2261")
    , ("< _"      , '≤'     , "U2264")
    , ("> _"      , '≥'     , "U2265")
    , ("< >"      , '⋄'     , "U22c4")
    , (": ."      , '∴'     , "therefore")
    , (". :"      , '∵'     , "because")
    , ("[ ]"      , '⌷'     , "U2337")
    , ("/ -"      , '⌿'     , "U233f")
    , ("\\ -"     , '⍀'     , "U2340")
    , ("_ '"      , '⍘'     , "U2358")
    , ("0 ~"      , '⍬'     , "U236c")
    , ("| ~"      , '⍭'     , "U236d")
    , ("c /"      , '¢'     , "cent" )
    , ("< _"      , '≤'     , "U2264")
    , ("> _"      , '≥'     , "U2265")
    , ("a r h"       , 'ء'  , "Arabic Letter Hamza")
    , ("a r a"       , 'ا'  , "Arabic Letter Alef")
    , ("a r a m a"   , 'آ'  , "Arabic Letter Alef with Madda Above")
    , ("a r a h a"   , 'أ'  , "Arabic Letter Alef with Hamza Above")
    , ("a r a m b"   , 'إ'  , "Arabic Letter Alef with Hamza Below")
    , ("a r a m"     , 'ى'  , "Arabic Letter Alef Maksura")
    , ("a r b"       , 'ب'  , "Arabic Letter Beh")
    , ("a r p"       , 'پ'  , "Arabic Letter Peh ")
    , ("a r t"       , 'ت'  , "Arabic Letter Teh")
    , ("a R t"       , 'ث'  , "Arabic Letter Theh")
    , ("a r t m"     , 'ة'  , "Arabic Letter Teh Marbuta")
    , ("a r j"       , 'ج'  , "Arabic Letter Jeem")
    , ("a r T"       , 'چ'  , "Arabic Letter Tcheh")
    , ("a R h"       , 'ح'  , "Arabic Letter Hah")
    , ("a r k"       , 'خ'  , "Arabic Letter Khah")
    , ("a r d"       , 'د'  , "Arabic Letter Dal")
    , ("a R T"       , 'ذ'  , "Arabic Letter Thal")
    , ("a r r"       , 'ر'  , "Arabic Letter Reh")
    , ("a r z"       , 'ز'  , "Arabic Letter Zain")
    , ("a r s"       , 'س'  , "Arabic Letter Seen")
    , ("a R s"       , 'ش'  , "Arabic Letter Sheen")
    , ("a r S"       , 'ص'  , "Arabic Letter Sad")
    , ("a R d"       , 'ض'  , "Arabic Letter Dad")
    , ("A r t"       , 'ط'  , "Arabic Letter Tah")
    , ("a r Z"       , 'ظ'  , "Arabic Letter Zah")
    , ("a r A"       , 'ع'  , "Arabic Letter Ain")
    , ("a r g"       , 'غ'  , "Arabic Letter Ghain")
    , ("a r f"       , 'ف'  , "Arabic Letter Feh")
    , ("a r v"       , 'ڤ'  , "Arabic Letter Veh")
    , ("a r q"       , 'ق'  , "Arabic Letter Qaf")
    , ("a R k"       , 'ك'  , "Arabic Letter Kaf")
    , ("a r l"       , 'ل'  , "Arabic Letter Lam")
    , ("a r l a"     , 'ﻻ'  , "Arabic Ligature Lam with Alef")
    , ("a r l a h a" , 'ﻷ'  , "Arabic Ligature Lam with Alef with Hamza Above")
    , ("a r l a h b" , 'ﻹ'  , "Arabic Ligature Lam with Alef with Hamza Below")
    , ("a r l a m a" , 'ﻵ'  , "Arabic Ligature Lam with Alef with Madda Above")
    , ("a r m"       , 'م'  , "Arabic Letter Meem")
    , ("a r n"       , 'ن'  , "Arabic Letter Noon")
    , ("a r H"       , 'ه'  , "Arabic Letter Heh")
    , ("a r w"       , 'و'  , "Arabic Letter Waw")
    , ("a r w h a"   , 'ؤ'  , "Arabic Letter Waw with Hamza Above")
    , ("a r y"       , 'ي'  , "Arabic Letter Yeh")
    , ("a r y h a"   , 'ئ'  , "Arabic Letter Yeh with Hamza Above")
    , ("A R T"       , ' ٌ'  , "Arabic Tanwin")
    , ("a r t a"     , ' ً'  , "Arabic Tanwin Above")
    , ("a r t b"     , ' ٍ'  , "Arabic Tanwin Below")
    , ("A r s"       , ' ْ'  , "Arabic Sukoon")
    , ("A R s"       , ' ّ'  , "Arabic Shadda")
    , ("a r D"       , ' ُ'  , "Arabic Damma")
    , ("a r F"       , ' َ'  , "Arabic Fatha")
    , ("a r K"       , ' ِ'  , "Arabic Kasra")
    , ("a r c"       , '،'  , "Arabic Comma")
    , ("a r ;"       , '؛'  , "Arabic Semicolon")
    , ("a r -"       , 'ـ'  , "Arabic Kashida")
    , ("a r ?"       , '؟'  , "Arabic Question Mark")

    -- Sequences that should exist but do not work
    --, ("^ spc", '^', "asciicircum") -- This overlaps with the normal 'shifted-6' macro for
    -- , ("' j", 'j́', "jacute")
    ]
