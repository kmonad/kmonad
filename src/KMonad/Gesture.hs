-- |

module KMonad.Gesture

where

import KMonad.Parsing

import Control.Monad.State

import qualified RIO.Seq  as Q
import qualified RIO.Set  as S


--------------------------------------------------------------------------------

data Toggle a = On a | Off a deriving (Eq, Show, Functor)

-- | A sequence of toggle-changes guaranteed to be valid
newtype Gesture a = Gesture { _gesture :: Q.Seq (Toggle a) }
  deriving (Eq, Show, Functor)

instance Semigroup (Gesture a) where
  (Gesture a) <> (Gesture b) = Gesture $ a <> b
instance Monoid (Gesture a) where
  mempty = Gesture Q.empty

-- | All the ways a '[Toggle a]' can be an invalid 'Gesture'
data GestureError a
  = OffWithoutOn a -- ^ An Off not preceded by an On
  | OnWithoutOff a -- ^ An On not succeeded by an Off
  deriving (Eq, Show)


--------------------------------------------------------------------------------

-- | A lens into the i
tag :: Lens' (Toggle a) a
tag f (On  x) = On  <$> f x
tag f (Off x) = Off <$> f x

-- | A fold of all the unique elements in a gesture
tags :: Ord a => Fold (Gesture a) a
tags = folding $ \(Gesture as) -> toList . S.fromList $ as^..folded.tag

-- | Create a tapping gesture
tap :: a -> Gesture a
tap a = Gesture . Q.fromList $ [On a, Off a]

-- | Wrap a gesture in a toggle iff the id does not already occur
around :: Ord a => a -> Gesture a -> Either (GestureError a) (Gesture a)
around x g@(Gesture seq)
  | anyOf tags (== x) g = Left $ OnWithoutOff x
  | otherwise = Right . Gesture $ (On x <| seq) |> Off x

-- | Create a gesture from a list of toggles
fromList :: Ord a => [Toggle a] -> Either (GestureError a) (Gesture a)
fromList as = case (`runState` S.empty) . runExceptT . foldM f Q.empty $ as of
  (Left e, _) -> Left e
  (Right g, s) -> case S.lookupMin s of
    Just toggle -> Left $ OnWithoutOff toggle
    Nothing     -> Right $ Gesture g
  where
    f s x = do
      pressed <- get
      case x of
        On c  | c `S.member` pressed -> throwError $ OnWithoutOff c
        On c -> put (S.insert c pressed) >> pure (s |> On c)
        Off c | not (c `S.member` pressed) -> throwError $ OffWithoutOn c
        Off c -> put (S.delete c pressed) >> pure (s |> Off c)

--------------------------------------------------------------------------------

type Gest = Q.Seq (Toggle Text)

data GestureReadError
  = GestureParseError ParseError
  | GestureValidateError (GestureError Text)
  deriving Eq

instance Show GestureReadError where
  show (GestureParseError e) = show e
  show (GestureValidateError e) = show e

instance Exception GestureReadError

-- | Parse a Gesture straight from Text
prsGesture :: Text -> Either GestureReadError (Gesture Text)
prsGesture t = case runParser gest "" t of
  Left e -> Left . GestureParseError . ParseError $ e
  Right gs -> case fromList (toList gs) of
    Left e -> Left . GestureValidateError $ e
    Right g -> pure g

-- | Characters that may not occur in tag-names
reserved :: [Char]
reserved = "()-~[]"

-- | Parse a series of valid characters as a tag
tag_ :: Parser Text
tag_ = takeWhile1P (Just "tag-character") f
  where f c = not $ isSpace c || c `elem` reserved

-- | Parse a "S-" sequence as 1 tag around another
around_ :: Parser Gest
around_ = do
  a <- tag_
  _ <- char '-'
  b <- try around_ <|> subg <|> tap_
  pure $ (On a <| b) |> Off a

-- | Parse a ")-X" as an OFF-toggle
closeTag :: Parser Gest
closeTag = do
  _ <- string ")-"
  a <- tag_
  pure . Q.singleton $ Off a

-- | Parse a "X-(" as an ON-toggle
openTag :: Parser Gest
openTag = do
  a <- tag_
  _ <- string "-("
  pure . Q.singleton $ On a

-- | Parse only a tag as a tap of that element
tap_ :: Parser Gest
tap_ = do
  a <- tag_
  pure . Q.fromList $ [On a, Off a]

-- | Parse a [] delimited series as a nested gesture
subg :: Parser Gest
subg = do
  _ <- char '['
  g <- gest
  _ <- char ']'
  pure g

-- | Parse a full gesture
gest :: Parser Gest
gest = do
  let one = lex . choice $ [subg, try openTag, try around_, try closeTag, tap_]
  es <- some one
  pure $ mconcat es
