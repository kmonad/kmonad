{- Some utilities for working with names of things -}
module KMonad.Util.Name

where

import KMonad.Prelude

import qualified RIO.HashMap as M

--------------------------------------------------------------------------------
-- $name
--
-- The base-type of how we deal with names

-- | When we talk about names, we use text
type Name = Text

-- | Maps of names to things
class HasName a where name :: Lens' a Name
instance HasName Name where name = id

--------------------------------------------------------------------------------
-- $lexicon
--
-- A lexicon is a collection of things that we have given names

-- | Model a 'Lexicon' using a HashMap
type Lexicon a = M.HashMap Name a

--------------------------------------------------------------------------------
-- $alias
--
-- How we provide alternative names for things
--
-- NOTE: We could consider generalizing the 'Alias' type to anything Hashable,
-- and then specializing our type to 'Alias Name'. But that is busywork for
-- another day.

-- | An alias binds a new name (_1) to an existing name (_2)
type Alias = (Name, Name)

--------------------------------------------------------------------------------
-- $err

-- | What can go wrong trying to insert aliases
data AliasError
  = NonExistentTarget Alias -- ^ Trying to alias to a non-existent name
  | NameAlreadyTaken  Alias -- ^ Trying to assign a name that is already taken
  deriving Show
makeClassyPrisms ''AliasError

instance Exception AliasError where
  displayException (NonExistentTarget (s, t)) = foldMap unpack $
    ["Failed to alias '",  s, "' to '", t, "': '", t, "' does not exist in lexicon"]
  displayException (NameAlreadyTaken (s, t)) = foldMap unpack $
    ["Failed to alias '",  s, "' to '", t, "': '", s, "' already exists in lexicon"]

instance AsAliasError SomeException where _AliasError = exception

--------------------------------------------------------------------------------
-- $util

-- | Insert an alias into a lexicon
insertAlias :: Alias -> Lexicon a -> Either AliasError (Lexicon a)
insertAlias (s, t) l
  | s `M.member` l = Left $ NameAlreadyTaken  (s, t)
  | otherwise = case M.lookup t l of
      Nothing -> Left  $ NonExistentTarget (s, t)
      Just v  -> Right $ M.insert s v l

