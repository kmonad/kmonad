{-# LANGUAGE ScopedTypeVariables #-}
module K.Gesture.Initial
  ( -- * Basic types
    Toggle(..)
  , Toggles
  , Gesture

    -- * Errors
  , AlternationError
  , AsAlternationError(..)
  , EmptinessError
  , AsEmptinessError(..)
  , GestureError
  , AsGestureError(..)

    -- * Smart constructors
  , mkSubgest
  , asGesture
  , mkGesture

    -- * Operations
  , togglesMap
  , togglesMapMaybe

    -- * Custom lenses
  , HasToggles(..)

  )
where

import K.Initial

import Control.Monad.State

import qualified RIO.Seq  as Q
import qualified RIO.Set  as S
import qualified RIO.Text as T

import qualified Control.Monad.Error.Lens as Err

-- basic types -----------------------------------------------------------------

-- | Either an 'On' or an 'Off' around some type
data Toggle a = On a | Off a
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Alias for a list of 'Toggle' of some type
type Toggles a = [Toggle a]

-- | A lens into the i
tag :: Lens' (Toggle a) a
tag = lens get set
  where get (On x)    = x
        get (Off x)   = x
        set (On _) x  = On x
        set (Off _) x = Off x

-- | A sequence of 'On' and 'Off' toggles guaranteed to follow these constraints:
--
-- 1. 'On' and 'Off' 'Toggle's for any value of 'a' always alternate
-- 2. The first (and therefore any odd) occurence for any 'a' is always 'On'
-- 3. The last occurence for any 'a' is always 'Off'
--
-- 'Gesture' was inspired by how people interact with a keyboard: you start with
-- none of the keys pressed, then you express a gesture by pressing and
-- releasing keys in some pattern, and you end your gesture with all keys
-- released again.
--
-- NOTE: It might be interesting to note that any gesture that actually follows
-- these rules can be modelled without the use of 'Toggle', all odd occurences
-- of some value are guaranteed to be presses, and all even occurences are
-- guaranteed to be releases.
--
-- NOTE: Gesture is not a Functor. An arbitrary transformation of @a -> b@ could
-- turn different @a@s into the same @b@, causing a 'Gesture' to break its
-- alternation law. The only way to fmap a 'Gesture' is with an isomorphism.
newtype Gesture a = Gesture { _gesture :: Q.Seq (Toggle a) }
  deriving (Eq, Show)
makeLenses ''Gesture
makeClassyPrisms ''Gesture


instance Semigroup (Gesture a) where
  (Gesture a) <> (Gesture b) = Gesture $ a <> b
instance Monoid (Gesture a) where
  mempty = Gesture Q.empty

-- | A subsection of a 'Gesture'
--
-- This type keeps the rule of alternation, but relaxes the 2nd and 3d rules
-- about starting and ending in the empty-state. It therefore expresses
-- subsections of a 'Gesture'.
--
-- NOTE: It might be interesting to note that, etymologically, /suggestion/
-- comes from /sub/ + /gerere/, the latter also being the root of /gesture/.
-- Another name for 'Subgest' might therefore be 'Suggestion'.
data Subgest a = Subgest
  { _minSeat  :: S.Set a          -- ^ The minimum starting context for this gesture
  , _minCrown :: S.Set a          -- ^ The minimum ending context when finished
  , _backbone :: Q.Seq (Toggle a) -- ^ The toggle-manipulations made to the state
  } deriving (Eq, Show)
makeClassy ''Subgest

-- errors ----------------------------------------------------------------------

-- | Violations of the alternation law, errors for both 'Gesture' and 'Subgest'
--
-- NOTE: I am not sure how to carry around the type variable 'a' into the Error,
-- and then have that play nice with the classy-prisms. Therefore we render the
-- carrier-type into Text first. This has its own issues, but is more
-- manageable.
data AlternationError
  = OffTwiceError Text -- ^ 2 'Off's in a row
  | OnTwiceError Text  -- ^ 2 'On's in a row
  deriving Eq
makeClassyPrisms ''AlternationError

instance Show AlternationError where
  show (OffTwiceError t) =
    "Encountered <Off " <> unpack t <> "> twice in a row"
  show (OnTwiceError t) =
    "Encountered <On " <> unpack t <> "> twice in a row"

instance Exception AlternationError
instance AsAlternationError SomeException where _AlternationError = _SomeException

-- | Violations of the initial and final emptiness laws, error for 'Gesture'
--
-- NOTE: Same type-caveat as for 'AlternationError'
data EmptinessError
  = NonEmptyStart [Text] -- ^ Requires non-empty initialization context
  | NonEmptyEnd   [Text] -- ^ Produces non-empty termination context
  deriving Eq
makeClassyPrisms ''EmptinessError

instance Show EmptinessError where
  show (NonEmptyStart ts) =
    "Items do not start Off: " <> unpack (T.intercalate ", " ts)
  show (NonEmptyEnd ts) =
    "Items do not end Off: " <> unpack (T.intercalate ", " ts)

instance Exception EmptinessError
instance AsEmptinessError SomeException where _EmptinessError = _SomeException

data GestureError
  = GestureAlternationError AlternationError
  | GestureEmptinessError   EmptinessError
  deriving Eq
makeClassyPrisms ''GestureError

instance Show GestureError where
  show (GestureAlternationError e) = show e
  show (GestureEmptinessError e)   = show e

instance Exception GestureError
instance AsGestureError SomeException where _GestureError = _SomeException
instance AsAlternationError GestureError where _AlternationError = _GestureAlternationError
instance AsEmptinessError GestureError where _EmptinessError = _GestureEmptinessError

-- creation --------------------------------------------------------------------

-- | Little helper state for the 'mkSubgest' calculation
data T a = T { _init :: S.Set a, _pressed :: S.Set a }
makeLenses ''T

-- | Create a 'Subgest' from a series of 'Toggle's.
mkSubgest :: (AsAlternationError e, MonadError e m, HasToggles g a)
  => g -> m (Subgest a)
mkSubgest g = do
{- This might be hard to follow, so let me explain the calculation carefully:

We receive a sequence of toggles, and are going to move through them one by one,
making a decision at every step to throw an error or manipulate some state.

We keep track of 2 sets: the set of currently pressed elements, and the set of
elements that must have been pressed before we got this sequence, otherwise
there would be 2 offs in a row. E.g. if a sequence starts with 'Off 1', then
that sequence only makes sense in the context of '1' already having been pressed.

So: if we get an On we add it to our pressed set, if it's already there, that
means an on-twice error. If we get an Off, if it's pressed, we just unpress it.
If its unpressed we try to add it to our initial context, if it's already there,
that's an Off-Twice error.
-}
  t <-  (`execStateT` T S.empty S.empty) . forM_ (g^..toggles) $ \case
    On a -> uses pressed (S.member a) >>= \case        -- If pressing
      True  -> errThrowing _OnTwiceError (tshow a)    --   but on: error twice on
      False -> pressed %= S.insert a                   --   and off: add to pressed
    Off a -> uses pressed (S.member a) >>= \case       -- If releasing
      True -> pressed %= S.delete a                    --   and on: delete from pressed
      False -> uses init (S.member a) >>= \case        --   but off: try add to seat
        True -> errThrowing _OffTwiceError (tshow a)  --     already part: error twice off
        False -> init %= S.insert a                    --     otherwise, add to seat
  pure $ Subgest (t^.init) (t^.pressed) (Q.fromList . toList $ g^..toggles)

-- | Create a 'Gesture' from a 'Subgest'
asGesture :: (AsEmptinessError e, MonadError e m, Show a)
  => Subgest a -> m (Gesture a)
asGesture s | s^.minSeat . to (not . S.null)
  = errThrowing _NonEmptyStart (map tshow . toList $ s^.minSeat)
asGesture s | s^.minCrown . to (not . S.null)
  = errThrowing _NonEmptyEnd (map tshow . toList $ s^.minCrown)
asGesture s = pure . Gesture $ s^.backbone

-- | Create a 'Gesture' from a 'Foldable' of 'Toggle's
mkGesture :: (AsGestureError e, MonadError e m, HasToggles g a)
  => g -> m (Gesture a)
mkGesture g = case mkSubgest g >>= asGesture of
  Left e -> errThrowing _GestureError e
  Right x -> pure x


-- custom lenses ---------------------------------------------------------------

-- | All 'Gesture's are 'Subgest', some 'Subgest' are 'Gesture'
instance Show a => AsGesture (Subgest a) a where
  __Gesture = prism' embed extract
    where
      embed   g = Subgest S.empty S.empty (g^.gesture)
      extract :: Subgest a -> Maybe (Gesture a)
      extract s = case asGesture s of
        Left (_ :: EmptinessError) -> Nothing
        Right x -> Just x

-- | A class describing things that have access to 0 or more toggles
class (Ord a, Show a) => HasToggles t a where toggles :: Fold t (Toggle a)

instance (Ord a, Show a, Foldable t) => HasToggles (t (Toggle a)) a where toggles = folded
instance (Ord a, Show a) => HasToggles (Subgest a) a where toggles = backbone.folded
instance (Ord a, Show a) => HasToggles (Gesture a) a where toggles = gesture.folded

-- translation -----------------------------------------------------------------

-- | Perform some @a -> b@ mapping over the toggles in some value
togglesMap :: HasToggles g a => (a -> b) -> g -> [Toggle b]
togglesMap f g = fmap (fmap f) (g^..toggles)

-- | Perform some @a -> Maybe b@ mapping over the toggles in some value
--
-- If any of the 'a's evaluates to Nothing, return the offending key in the Left
-- field of the either. Otherwise, return a list of all succesful remappings.
togglesMapMaybe :: HasToggles g a => (a -> Maybe b) -> g -> Either a [Toggle b]
togglesMapMaybe f s = left (view tag) res
  where res = traverse (reportFail . mapM $ f) $ s^..toggles
