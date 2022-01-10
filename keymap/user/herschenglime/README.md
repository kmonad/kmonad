# QMK-Like autoshift implementation

I initially created a keymap to implement autoshift around 6 months ago, but didn't feel comfortable sharing it as it had issues when rolling in between keys due to the nature of how KMonad handles shifted keycodes. I finally figured out a solution I'm happy with by using tap macros to ensure that the shifted key is immediately tapped and released. It has the added caveat of disabling key repeat for letter and symbol keys, but I don't see much use in repeatedly sending AAAAAAAA anyways. 

The only other difference from stock is that I've replaced caps lock with delete, but this of course can be changed. Feel free to extract the list of aliases to use within your own keymap; hopefully this saves you the trouble of manually setting up the tap-holds. The 145 ms delay may be a bit short to begin with, but is easily changed to a desired value with a find and replace.  
