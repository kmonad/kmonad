let Keycode = Natural

let CoreName = Text

let Names = List Text

let Gesture = Text

let Option = Text

let Flag = Text

let Keycodes = List { mapKey : CoreName, mapValue : Keycode }

let KAliases = List { mapKey : CoreName, mapValue : Names }

let Gestures = List { mapKey : CoreName, mapValue : Gesture }

let GAliases = List { mapKey : CoreName, mapValue : Names }

let Options = List { mapKey : Text, mapValue : Option }

let Flags = List Flag

let CfgFile =
      { keycodes : Keycodes
      , kaliases : KAliases
      , gestures : Gestures
      , galiases : GAliases
      , options : Options
      , flags : Flags
      }

let empty =
        { keycodes = [] : Keycodes
        , kaliases = [] : KAliases
        , gestures = [] : Gestures
        , galiases = [] : GAliases
        , options = [] : Options
        , flags = [] : Flags
        }
      : CfgFile

in  { Keycode
    , CoreName
    , Names
    , Gesture
    , Option
    , Flag
    , Keycodes
    , KAliases
    , Gestures
    , GAliases
    , Options
    , Flags
    , CfgFile
    , empty
    }
