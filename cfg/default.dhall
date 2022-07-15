let T = ./types.dhall

let L = ./locale/keycodes/linux_enUS.dhall

let A = ./locale/aliases/aliases.dhall

let empty =
        { keycodes = [] : T.Keycodes
        , kaliases = [] : T.KAliases
        , gestures = [] : T.Gestures
        , galiases = [] : T.GAliases
        , options = [] : T.Options
        , flags = [] : T.Flags
        }
      : T.CfgFile

let linux = empty // { keycodes = toMap L.codenames } : T.CfgFile

in  { linux, empty }
