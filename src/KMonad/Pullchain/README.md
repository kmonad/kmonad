This module is getting prepared to be completely refactored.

The first step is separating 'App' from 'Model' altogether.

The second step is separating semantics that we intend to support (Layers,
various complicated buttons) from implementation.

The third step is generating a different implementation for the same semantics.


Until all 3 steps are completed, the old implementation will live in 'Pullchain'

'Model' will start to receive token-semantics types. 'Pullchain' will use these
token-semantics types to configure its own running behavior. Finally, the new model will also use these token-semantics types for a better implementation.

