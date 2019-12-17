The Domain subdirectory of src is intended for code that is specific to KMonad.
Code that lives here is supposed to not depend on concrete Monad's, but instead
use the 'HasXXX' pattern constraints to describe the required effects on a
per-function basis.
