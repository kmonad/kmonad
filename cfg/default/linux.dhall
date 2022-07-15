let T = ../initial.dhall

let L = ../locale/keycodes/linux_enUS.dhall

let G = ../locale/gestures/basic_enUS.dhall

let A = ../locale/aliases.dhall

let linux =
      { keycodes = toMap L.keycodes
      , kaliases = toMap A.C.all
      , gestures = toMap G.shifted
      , galiases = toMap A.G.all
      }

in  { linux = T.empty // linux : T.CfgFile }
