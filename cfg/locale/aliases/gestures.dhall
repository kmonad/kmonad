let max4 =
      { exclamationmark = [ "excl" ]
      , dollar = [ "doll", "cash" ]
      , percent = [ "perc" ]
      , ampersand = [ "ampr", "and" ]
      , asterisk = [ "star", "astr", "glob" ]
      , leftparen = [ "lprn", "prnL" ]
      , rightparen = [ "rprn", "prnR" ]
      , underscore = [ "uscr" ]
      , leftcurly = [ "lcrl", "crlL" ]
      , rightcurly = [ "rcrl", "crlR" ]
      , colon = [ "cln", "coln" ]
      , doublequote = [ "dqot" ]
      , lesserthan = [ "less", "angL" ]
      , greaterthan = [ "more", "angR" ]
      , questionmark = [ "que" ]
      }

let symbols =
      { ampersand = [ "_&" ]
      , asterisk = [ "_*" ]
      , colon = [ "_:" ]
      , dollar = [ "_\$" ]
      , doublequote = [ "_\"" ]
      , exclamationmark = [ "_!" ]
      , greaterthan = [ "_>" ]
      , leftcurly = [ "_{" ]
      , leftparen = [ "_(" ]
      , lesserthan = [ "_<" ]
      , questionmark = [ "_?" ]
      , rightcurly = [ "_}" ]
      , rightparen = [ "_)" ]
      , underscore = [ "__" ]
      }

let escaped =
      { leftparen = [ "_\\(" ]
      , rightparen = [ "_\\)" ]
      , doublequote = [ "_\\\"" ]
      }

let all = max4 // symbols // escaped

in  { max4, symbols, escaped, all }
