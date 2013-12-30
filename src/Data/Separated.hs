module Data.Separated(
  Separated(..)
, single
, separatedValues1
, separatedValues
, separatedHead
, separatedTail
, separators
) where

import Prelude(Eq, Ord, Show(..), Functor(..), Monad(..), fst, snd, zipWith, (.))
import Data.List.NonEmpty(NonEmpty(..), toList)
import Control.Applicative(Applicative(..))
import Control.Lens(Lens', lens)
import Data.Semigroup(Semigroup(..))

-- $setup
-- >>> import Prelude
-- >>> import Control.Lens
-- >>> import Test.QuickCheck
-- >>> instance (Arbitrary a, Arbitrary s) => Arbitrary (Separated s a) where arbitrary = do a <- arbitrary; x <- arbitrary; return (Separated a x)

data Separated s a =
  Separated a [(s, a)]
  deriving (Eq, Ord)

instance (Show s, Show a) => Show (Separated s a) where
  show (Separated a x) =
    '[' : show a <> (x >>= \(s, y) -> show s <> show y) <> "]"

-- |
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

instance Semigroup s => Applicative (Separated s) where
  pure =
    single
  Separated a x <*> Separated a' x' =
    Separated (a a') (zipWith (\(s, t) (u, v) -> (s <> u, t v)) x x')

-- |
--
-- prop> single x ^. separatedTail == []
single ::
  a
  -> Separated s a
single a =
  Separated a []

separatedValues1 ::
  Separated s a
  -> NonEmpty a
separatedValues1 (Separated a x) =
  a :| fmap snd x

separatedValues ::
  Separated s a
  -> [a]
separatedValues =
  toList . separatedValues1

separatedHead ::
  Lens' (Separated s a) a
separatedHead =
  lens (\(Separated a _) -> a) (\(Separated _ x) a -> Separated a x)

separatedTail ::
  Lens' (Separated s a) [(s, a)]
separatedTail =
  lens (\(Separated _ x) -> x) (\(Separated a _) x -> Separated a x)

separators ::
  Separated s a
  -> [s]
separators (Separated _ x) =
  fmap fst x
