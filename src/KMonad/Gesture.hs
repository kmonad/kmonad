-- |

module KMonad.Gesture

where


import KMonad.Prelude hiding (try)

import System.IO

import Control.Arrow (right)
import Control.Monad.Except
import Control.Monad.State

import RIO.List.Partial (head)
import RIO.Seq (Seq(..))

import qualified RIO.List as L
import qualified RIO.Seq  as Q
import qualified RIO.Set  as S

import Data.Char

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as X



--------------------------------------------------------------------------------

data Toggle a = On a | Off a deriving (Eq, Show, Functor)

code :: Lens' (Toggle a) a
code = lens get set
  where get (On x)    = x
        get (Off x)   = x
        set (On _) x  = On x
        set (Off _) x = Off x

newtype Gesture a = Gesture { _gesture :: Q.Seq (Toggle a) }
  deriving (Eq, Show, Functor)

instance Semigroup (Gesture a) where
  (Gesture a) <> (Gesture b) = Gesture $ a <> b
instance Monoid (Gesture a) where
  mempty = Gesture $ Q.empty

-- | A fold of all the unique elements in a gesture
--
-- NOTE: Non-optimized non-bottleneck
elems :: Ord a => Fold (Gesture a) a
elems = folding $ \(Gesture as) -> toList . S.fromList $ as^..folded.code

-- | All the ways a '[Toggle a]' can be an invalid 'Gesture'
data GestureError a
  = OffWithoutOn a -- ^ An Off not preceded by an On
  | OnWithoutOff a -- ^ An On not succeeded by an Off
  deriving (Eq, Show)

-- | Create a tapping gesture
tap :: a -> Gesture a
tap a = Gesture . Q.fromList $ [On a, Off a]

-- | Wrap a gesture in a toggle iff the id does not already occur
around :: Ord a => a -> Gesture a -> Either (GestureError a) (Gesture a)
around x g@(Gesture seq)
  | anyOf elems (== x) g = Left $ OnWithoutOff x
  | otherwise = Right . Gesture $ (On x <| seq) |> Off x

-- | Create a gesture from a list of toggles
fromList :: Ord a => [Toggle a] -> Either (GestureError a) (Gesture a)
fromList as = case (`runState` S.empty) . runExceptT . foldM f Q.empty $ as of
  (Left e, _) -> Left e
  (Right g, s) | S.null s -> Right $ Gesture g
               | otherwise -> Left $ OnWithoutOff (head . S.elems $ s)
  where
    f s x = do
      pressed <- get
      case x of
        On c  | c `S.member` pressed -> throwError $ OnWithoutOff c
        On c -> put (S.insert c pressed) >> pure (s |> On c)
        Off c | not (c `S.member` pressed) -> throwError $ OffWithoutOn c
        Off c -> put (S.delete c pressed) >> pure (s |> Off c)


foo, bar, baz, bop, bup :: Either (GestureError Int) (Gesture Int)
foo = fromList $ [On 1, On 2, Off 1, Off 2]
bar = fromList $ [Off 3, On 1, On 2, Off 1, Off 2]
baz = fromList $ [On 1, On 2, Off 1]
bop = fromList $ [On 1, On 1, Off 1]
bup = fromList $ [On 1, Off 1, Off 1]

--------------------------------------------------------------------------------

{-
"a b c"
"S-a"
"S-(a b c)-S"
-}

-- CONTINUE HERE
newtype ParseErrorG = ParseErrorG { _gbundle :: ParseErrorBundle }

type G a = ExceptT

type Parser = Parsec Void Text
type Gest = Q.Seq (Toggle Text)

readGesture :: Text -> Maybe (Gesture Text)
readGesture t = case runParser gest "" t of
  Left _ -> Nothing
  Right gs ->
  either (const Nothing) Just . right fromList .

parseG :: Show a => Parser a -> Text -> IO ()
parseG a t = case runParser a "" t of
  Left perr -> putStr (errorBundlePretty perr)
  Right x   -> print x

reserved :: [Char]
reserved = "()-~[]"

sc :: Parser ()
sc = X.space space1 empty empty

lex :: Parser a -> Parser a
lex = X.lexeme sc

tag :: Parser Text
tag = takeWhile1P (Just "tag-character") f
  where f c = not $ isSpace c || c `elem` reserved

around_ :: Parser Gest
around_ = do
  a <- tag
  _ <- char '-'
  b <- try around_ <|> subg <|> tap_

  pure $ (On a <| b) |> Off a

closeTag :: Parser Gest
closeTag = do
  _ <- string ")-"
  a <- tag
  pure . Q.singleton $ Off a

openTag :: Parser Gest
openTag = do
  a <- tag
  _ <- string "-("
  pure . Q.singleton $ On a

tap_ :: Parser Gest
tap_ = do
  a <- tag
  pure . Q.fromList $ [On a, Off a]

subg :: Parser Gest
subg = do
  _ <- char '['
  g <- gest
  _ <- char ']'
  pure g

gest :: Parser Gest
gest = do
  let one = lex . choice $ [subg, try openTag, try around_, try closeTag, tap_]
  es <- some one
  pure $ mconcat es
