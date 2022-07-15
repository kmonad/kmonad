-- |

module K.Gesture.Ops where


import K.Initial
import K.Gesture.Initial
import K.Initial.Parsing

import Control.Monad.Except
import Control.Monad.State

import RIO.List.Partial (head)
import RIO.Seq (Seq(..))

import qualified RIO.List as L
import qualified RIO.Set  as S
import qualified RIO.Seq  as Q

-- FIXME

-- ops -------------------------------------------------------------------------

-- | A lens into the i
tag :: Lens' (Toggle a) a
tag = lens get set
  where get (On x)    = x
        get (Off x)   = x
        set (On _) x  = On x
        set (Off _) x = Off x

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
--
-- TODO: Redo me, this should be MonadError and classy prisms
fromList :: (Foldable f, Ord a)
  => f (Toggle a)
  -> Either (GestureError a) (Gesture a)
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

-- -- | Parse a Gesture straight from Text
-- readGesture :: Text -> Either GestureExprError (Gesture Text)
-- readGesture t = case runParser tseq "" t of
--   Left e -> Left . GestureParseError . ParseError $ e
--   Right gs -> case fromList (toList gs) of
--     Left e -> Left . GestureValidateError $ e
--     Right g -> pure g
