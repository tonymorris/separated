{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Separated.Separated(
  Separated
, separated
, Separated1
, separated1
, separated1Head
, separated1Tail
, empty
, (+-)
, single
, shift
, separatedSwap
) where

import Control.Applicative(Applicative((<*>), pure))
import Control.Category(Category(id, (.)))
import Control.Lens.Iso(Iso, iso, from)
import Control.Lens.Tuple(_1, _2)
import Control.Lens.Type(Lens)
import Data.Functor(Functor(fmap))
import Data.Functor.Apply(Apply((<.>)))
import Data.Bifunctor(Bifunctor(bimap))
import Data.Eq(Eq)
import Data.List(intercalate, zipWith, repeat)
import Data.Monoid(Monoid(mappend, mempty))
import Data.Ord(Ord)
import Data.Semigroup(Semigroup((<>)))
import Data.Separated.SeparatedCons(SeparatedCons((+:), SeparatedConsF, SeparatedConsG))
import Data.String(String)
import Prelude(Show(show))

-- $setup
-- >>> :set -XNoImplicitPrelude
-- >>> import Control.Lens.Review((#))
-- >>> import Control.Monad(Monad(return))
-- >>> import Data.Char(toUpper)
-- >>> import Data.Eq(Eq((==)))
-- >>> import Data.List(reverse, drop)
-- >>> import Control.Lens(set, (^.))
-- >>> import Prelude(Num(..), String, Int)
-- >>> import Test.QuickCheck(Arbitrary(..))
-- >>> instance (Arbitrary s, Arbitrary a) => Arbitrary (Separated s a) where arbitrary = fmap (^. separated) arbitrary
-- >>> instance (Arbitrary a, Arbitrary s) => Arbitrary (Separated1 s a) where arbitrary = do a <- arbitrary; x <- arbitrary; return ((a, x) ^. separated1)

data Separated s a =
  Separated [(s, a)]
  deriving (Eq, Ord)

instance Bifunctor Separated where
  bimap f g (Separated x) =
    Separated (fmap (\(s, a) -> (f s, g a)) x)

-- | Map across a @Separated@ on the element values.
--
-- prop> fmap id (x :: Separated Int String) == x
--
-- prop> fmap (+1) (a +: b +: empty) == a +: (1+b) +: empty
instance Functor (Separated s) where
  fmap =
    bimap id

-- | Applies functions with element values, using a zipping operation, appending
-- separators.
--
-- >>> (empty :: Separated [Int] (String -> [String])) <.> empty
-- []
--
-- >>> [1,2] +: (\s -> [s, reverse s, drop 1 s]) +: empty <.> [3,4,5] +: "abc" +: empty
-- [[1,2,3,4,5],["abc","cba","bc"]]
instance Semigroup s => Apply (Separated s) where
  (<.>) =
    separatedAp (<>)

-- | Applies functions with element values, using a zipping operation, appending
-- separators. The identity operation is an infinite list of the empty separator
-- and the given element value.
--
-- >>> (empty :: Separated [Int] (String -> [String])) <*> empty
-- []
--
-- >>> [1,2] +: (\s -> [s, reverse s, drop 1 s]) +: empty <*> [3,4,5] +: "abc" +: empty
-- [[1,2,3,4,5],["abc","cba","bc"]]
instance Monoid s => Applicative (Separated s) where    
  (<*>) =
    separatedAp mappend
  pure =
    Separated . repeat . (,) mempty

instance (Show s, Show a) => Show (Separated s a) where
  show (Separated x) =
    showSeparated id x

instance Semigroup (Separated s a) where
  Separated x <> Separated y =
    Separated (x <> y)    

instance Monoid (Separated s a) where
  mappend =
    (<>)
  mempty =
    Separated mempty

instance SeparatedCons Separated1 Separated where
  type SeparatedConsF Separated = Separated1
  type SeparatedConsG Separated1 = Separated
  s +: Separated1 a (Separated x) =
    Separated ((s, a) : x)

-- | The isomorphism to a list of pairs of element and separator values.
--
-- >>> separated # empty
-- []
--
-- >>> separated # ('x' +: 6 +: empty)
-- [('x',6)]
--
-- >>> [] ^. separated
-- []
--
-- >>> [(6, [])] ^. separated
-- [6,[]]
separated ::
  Iso [(s, a)] [(t, b)] (Separated s a) (Separated t b)
separated =
  iso Separated (\(Separated x) -> x)

----

data Separated1 a s =
  Separated1 a (Separated s a)
  deriving (Eq, Ord)

instance Bifunctor Separated1 where
  bimap f g (Separated1 a x) =
    Separated1 (f a) (bimap g f x)

-- | Map across a @Separated1@ on the separator values.
--
-- >>> fmap (+1) (set separated1Tail (1 +: 'b' +: 2 +: 'c' +: empty) (single 'a'))
-- ['a',2,'b',3,'c']
--
-- prop> fmap id (x :: Separated1 Int String) == x
--
-- prop> fmap (+1) (single x) == single x
instance Functor (Separated1 s) where
  fmap =
    bimap id

-- | Applies functions with separator values, using a zipping operation,
-- appending elements.
--
-- >>> [1,2] +: reverse +: [3,4] +: empty <.> [5,6,7] +: "abc" +: [8] +: empty
-- [[1,2,5,6,7],"cba",[3,4,8]]
instance Semigroup s => Apply (Separated1 s) where
  (<.>) =
    separated1Ap (<>)

instance (Show a, Show s) => Show (Separated1 a s) where
  show (Separated1 a (Separated x)) =
    showSeparated (show a:) x
    
-- | Applies functions with separator values, using a zipping operation,
-- appending elements. The identity operation is an infinite list of the empty
-- element and the given separator value.
--
-- >>> [1,2] +: reverse +: [3,4] +: empty <*> [5,6,7] +: "abc" +: [8] +: empty
-- [[1,2,5,6,7],"cba",[3,4,8]]
instance Monoid s => Applicative (Separated1 s) where    
  (<*>) =
    separated1Ap mappend
  pure =
    Separated1 mempty . swapSeparated . pure

-- | The isomorphism to element values interspersed with a separator.
--
-- >>> separated1 # (single 6)
-- (6,[])
--
-- >>> separated1 # (5 +: 'x' +: single 6)
-- (5,['x',6])
--
-- >>> (6, empty) ^. separated1
-- [6]
--
-- >>> (5, 'x' +- 6) ^. separated1
-- [5,'x',6]
separated1 ::
  Iso (a, Separated s a) (b, Separated t b) (Separated1 a s) (Separated1 b t)
separated1 =
  iso (\(a, x) -> Separated1 a x) (\(Separated1 a x) -> (a, x))

instance SeparatedCons Separated Separated1 where
  type SeparatedConsF Separated1 = Separated
  type SeparatedConsG Separated = Separated1
  (+:) =
    Separated1

-- | A lens on the first element value.
--
-- >>> single 7 ^. separated1Head
-- 7
--
-- prop> single x ^. separated1Head == (x :: Int)
separated1Head ::
  Lens (Separated1 a t) (Separated1 a t) a a
separated1Head =
  from separated1 . _1

-- | A lens on the tail.
--
-- prop> (d +: e +: single x) ^. separated1Tail == e +: x +: empty
separated1Tail ::
  Lens (Separated1 a s) (Separated1 a t) (Separated s a) (Separated t a)
separated1Tail =
  from separated1 . _2

----

empty ::
  Separated s a
empty =
  Separated []

-- | One element and one separator.
--
-- >>> 7 +- "abc"
-- [7,"abc"]
--
-- >>> 7 +: "abc" +: 8 +- "def"
-- [7,"abc",8,"def"]
(+-) ::
  s
  -> a
  -> Separated s a
s +- a =
  s +: single a

-- | Zero element values interspersed with one element.
--
-- >>> single 4
-- [4]
--
-- prop> single x ^. separated1Tail == empty
single ::
  a
  -> Separated1 a s
single a =
  Separated1 a empty

-- | The isomorphism that shuffles the elements and separators one position.
--
-- >>> shift # ([], 6)
-- [6]
--
-- >>> shift # ([(5, 'x')], 6)
-- [5,'x',6]
--
-- >>> single 6 ^. shift
-- ([],6)
--
-- >>> (5 +: 'x' +: single 6) ^. shift
-- ([(5,'x')],6)
shift ::
  Iso (Separated1 a s) (Separated1 b t) ([(a, s)], a) ([(b, t)], b)
shift =
  let shiftR ([], a) =
        Separated1 a (Separated [])
      shiftR ((b, s):r, a) =
        let Separated1 z' (Separated w) = shiftR (r, b)
        in Separated1 z' (Separated ((s, a) : w))
      shiftL (Separated1 s' (Separated [])) =
        ([], s')
      shiftL (Separated1 s' (Separated ((a, t') : r))) =
        let (w, z) = shiftL (Separated1 t' (Separated r))
        in ((s', a) : w, z)
  in iso shiftL shiftR

-- | The isomorphism that swaps elements with their separators.
--
-- >>> separatedSwap # empty
-- []
--
-- >>> separatedSwap # ('x' +: 6 +: empty)
-- [6,'x']
--
-- >>> empty ^. separatedSwap
-- []
--
-- >>> ('x' +: 6 +: empty) ^. separatedSwap
-- [6,'x']
separatedSwap ::
  Iso (Separated s a) (Separated t b) (Separated a s) (Separated b t)
separatedSwap =
  let swap (a, b) = (b, a)
  in iso (\(Separated x) -> Separated (fmap swap x)) (\(Separated x) -> Separated (fmap swap x))

--- -- values, separators, lookup, FlipSeparated, combinators

----

showSeparated ::
 (Show a, Show s, Functor f) =>
 (f String -> [String])
 -> f (s, a)
 -> String
showSeparated f x =
  '[' : intercalate "," (f (fmap (\(s, a) -> show s <> "," <> show a) x)) <> "]"

separatedAp ::
  (s -> s -> s)
  -> Separated s (a -> b)
  -> Separated s a
  -> Separated s b
separatedAp op (Separated f) (Separated a) =
    Separated (zipWith (\(s, f') (t, a') -> (s `op` t, f' a')) f a)  

separated1Ap ::
  (a -> a -> a)
  -> Separated1 a (s -> t)
  -> Separated1 a s
  -> Separated1 a t
separated1Ap op (Separated1 f (Separated fs)) (Separated1 a (Separated as)) =
    Separated1 (f `op` a) (Separated (zipWith (\(s, f') (t, a') -> (s t, f' `op` a')) fs as))

swapSeparated ::
  Separated s a
  -> Separated a s
swapSeparated (Separated x) =
  Separated (fmap (\(s, a) -> (a, s)) x)
