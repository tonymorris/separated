module Data.Separated(
  Separated
, (~>)
, single
, separatedValues1
, separatedValues
, separatedHead
, separatedTail
, separators
) where

import Prelude(Eq, Ord, Show(..), Functor(..), Monad(..), fst, snd, zipWith, (.))
import Data.List.NonEmpty(NonEmpty(..), toList)
import Control.Lens(Lens', lens)
import Data.Semigroup(Semigroup(..))

-- $setup
-- >>> import Prelude(Eq(..), Num(..), String, Int, id)
-- >>> import Control.Lens(set, (^.))
-- >>> import Test.QuickCheck(Arbitrary(..))
-- >>> instance (Arbitrary a, Arbitrary s) => Arbitrary (Separated s a) where arbitrary = do a <- arbitrary; x <- arbitrary; return (Separated a x)

-- | A data type representing element values interspersed with a separator.
--
-- There is one fewer separator values (@s@) than there are element values (@a@). There is at least one element value.
data Separated s a =
  Separated a [(s, a)]
  deriving (Eq, Ord)

instance (Show s, Show a) => Show (Separated s a) where
  show (Separated a x) =
    '[' : show a <> (x >>= \(s, y) -> show s <> show y) <> "]"

-- | Map across a @Separated@ on the element values.
--
-- prop> fmap id (x :: Separated Int String) == x
--
-- >>> fmap (+1) (single 1)
-- [2]
--
-- >>> fmap (+1) (set separatedTail [('a', 2), ('b', 3)] (single 1))
-- [2'a'3'b'4]
instance Functor (Separated s) where
  fmap f (Separated a x) =
    Separated (f a) (fmap (\(s, y) -> (s, f y)) x)

-- | Prepend a separator and element to the current tail.
--
-- >>> ('b', 9) ~> ('a', 8) ~> single 7
-- [7'b'9'a'8]
(~>) ::
  (s, a)
  -> Separated s a
  -> Separated s a
e ~> Separated a x =
  Separated a (e:x)

infixr 5 ~>

-- |
--
-- >>> single 4
-- [4]
--
-- prop> single x ^. separatedTail == []
single ::
  a
  -> Separated s a
single a =
  Separated a []

-- | Return all element values.
--
-- >>> separatedValues1 (single 8)
-- 8 :| []
--
-- >>> separatedValues1 (('a', 9) ~> single 8)
-- 8 :| [9]
--
-- prop> let h :| _ = separatedValues1 (single x) in h == (x :: Int)
--
-- prop> let _ :| t = separatedValues1 (e ~> single x) in t == fmap fst [e]
separatedValues1 ::
  Separated s a
  -> NonEmpty a
separatedValues1 (Separated a x) =
  a :| fmap snd x

-- | Return all element values.
--
-- >>> separatedValues (single 8)
-- [8]
--
-- >>> separatedValues (('a', 9) ~> single 8)
-- [8,9]
--
-- prop> let h : _ = separatedValues (single x) in h == (x :: Int)
--
-- prop> let _ : t = separatedValues (e ~> single x) in t == fmap fst [e]
separatedValues ::
  Separated s a
  -> [a]
separatedValues =
  toList . separatedValues1

-- | A lens on the first element value.
--
-- >>> single 7 ^. separatedHead
-- 7
--
-- prop> single x ^. separatedHead == (x :: Int)
separatedHead ::
  Lens' (Separated s a) a
separatedHead =
  lens (\(Separated a _) -> a) (\(Separated _ x) a -> Separated a x)

-- | A lens on the tail.
--
-- >>> single 7 ^. separatedHead
-- 7
--
-- prop> (e ~> single x) ^. separatedTail == [e]
separatedTail ::
  Lens' (Separated s a) [(s, a)]
separatedTail =
  lens (\(Separated _ x) -> x) (\(Separated a _) x -> Separated a x)

-- | Return all separator values.
--
-- >>> separators (('a', 8) ~> single 7)
-- "a"
--
-- >>> separators (('b', 9) ~> ('a', 8) ~> single 7)
-- "ba"
--
-- prop> separators (single x) == []
separators ::
  Separated s a
  -> [s]
separators (Separated _ x) =
  fmap fst x
