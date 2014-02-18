{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Separated(
  -- * Data types
  Separated
, Separated'
, Separated1
, Separated1'
  -- * Inserting elements
, SeparatedCons(..)
, (++:)
, (*+:)
, (**:)
  -- * Constructing data types
, empty
, single
, (+:.)
  -- * Extracting values from data types
, allValues
, allValues1
, separatedValues
, separated1Values
, separators
, separators1
  -- * Lenses and isomorphisms
, HasSeparated(..)
, HasFSeparated(..)
, HasSeparated1(..)
, HasFSeparated1(..)
, separatedIso
, separatedSwap
, separated1Iso
, shift
, separated1Head
, separated1Tail
  -- * Alternating combinators
, separatedWith
, separatedWith1
) where

import Prelude(Eq, Ord, Show(..), Functor(..), Monad(..), Bool(..), fst, snd, const, id, not, (.))
import Data.List.NonEmpty(NonEmpty(..))
import Data.List(intercalate, zipWith, repeat)
import Control.Lens(Lens', Iso', lens, iso, (#), (^.))
import Data.Semigroup(Semigroup(..))
import Data.Monoid(Monoid(..))
import Data.Functor((<$>))
import Data.Maybe(Maybe(..))
import Control.Applicative(Applicative(..), Alternative(many, (<|>)))
import Data.Functor.Apply(Apply(..))

-- $setup
-- >>> import Prelude(Eq(..), Num(..), String, Int, id)
-- >>> import Data.List(reverse, drop)
-- >>> import Control.Lens(set, (^.))
-- >>> import Test.QuickCheck(Arbitrary(..))
-- >>> instance (Arbitrary s, Arbitrary a) => Arbitrary (Separated s a) where arbitrary = fmap Separated arbitrary
-- >>> instance (Arbitrary a, Arbitrary s) => Arbitrary (Separated1 s a) where arbitrary = do a <- arbitrary; x <- arbitrary; return (Separated1 a x)

-- | A data type representing a list of pairs of separator and element values.
newtype Separated s a =
  Separated [(s, a)]
  deriving (Eq, Ord)

-- | Structures that view a @Separated@ through a lens.
class HasSeparated f where
  separatedL ::
    Lens' (f s a) (Separated s a)

-- | Structures that view a flipped @Separated@ through a lens.
class HasFSeparated f where
  fseparatedL ::
    Lens' (f s a) (Separated a s)

instance HasSeparated Separated where
  separatedL =
    id

type Separated' x =
  Separated x x

instance (Show s, Show a) => Show (Separated s a) where
  show (Separated x) =
    '[' : intercalate "," (fmap (\(s, a) -> show s <> "," <> show a) x) <> "]"

-- | Map across a @Separated@ on the element values.
--
-- prop> fmap id (x :: Separated Int String) == x
--
-- prop> fmap (+1) (a +: b +: empty) == a +: (1+b) +: empty
instance Functor (Separated s) where
  fmap f (Separated x) =
    Separated (fmap (\(a, b) -> (a, f b)) x)

-- not exported
separatedAp ::
  (s -> s -> s)
  -> Separated s (a -> b)
  -> Separated s a
  -> Separated s b
separatedAp op (Separated f) (Separated a) =
    Separated (zipWith (\(s, f') (t, a') -> (s `op` t, f' a')) f a)

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
  pure a =
    Separated (repeat (mempty, a))

-- | A data type representing element values interspersed with a separator.
--
-- There is one fewer separator values (@s@) than there are element values (@a@). There is at least one element value.
data Separated1 a s =
  Separated1 a [(s, a)]
  deriving (Eq, Ord)

-- | Structures that view a @Separated1@ through a lens.
class HasSeparated1 f where
  separated1L ::
    Lens' (f a s) (Separated1 a s)

-- | Structures that view a flipped @Separated1@ through a lens.
class HasFSeparated1 f where
  fseparated1L ::
    Lens' (f a s) (Separated1 s a)

instance HasSeparated1 Separated1 where
  separated1L =
    id

type Separated1' x =
  Separated1 x x

instance (Show a, Show s) => Show (Separated1 a s) where
  show (Separated1 a x) =
    '[' : intercalate "," (show a : fmap (\(s, a') -> show s <> "," <> show a') x) <> "]"

-- | Map across a @Separated1@ on the separator values.
--
-- >>> fmap (+1) (set separated1Tail (1 +: 'b' +: 2 +: 'c' +: empty) (single 'a'))
-- ['a',2,'b',3,'c']
--
-- prop> fmap id (x :: Separated1 Int String) == x
--
-- prop> fmap (+1) (single x) == single x
instance Functor (Separated1 a) where
  fmap f (Separated1 a x) =
    Separated1 a (fmap (\(s, y) -> (f s, y)) x)

-- not exported
separated1Ap ::
  (a -> a -> a)
  -> Separated1 a (s -> t)
  -> Separated1 a s
  -> Separated1 a t
separated1Ap op (Separated1 a f) (Separated1 b s) =
  Separated1 (a `op` b) (zipWith (\(f', s') (x, t') -> (f' x, s' `op` t')) f s)

-- | Applies functions with separator values, using a zipping operation,
-- appending elements.
--
-- >>> [1,2] +: reverse +: [3,4] +: empty <.> [5,6,7] +: "abc" +: [8] +: empty
-- [[1,2,5,6,7],"cba",[3,4,8]]
instance Semigroup a => Apply (Separated1 a) where
  (<.>) =
    separated1Ap (<>)

-- | Applies functions with element values, using a zipping operation, appending
-- elements. The identity operation is an infinite list of the empty element
-- and the given separator value.
--
-- >>> [1,2] +: reverse +: [3,4] +: empty <*> [5,6,7] +: "abc" +: [8] +: empty
-- [[1,2,5,6,7],"cba",[3,4,8]]
instance Monoid a => Applicative (Separated1 a) where
  (<*>) =
    separated1Ap mappend
  pure a =
    Separated1 mempty (repeat (a, mempty))

-- | Prepend a value to a separated-like structure.
--
-- >>> 'z' +: empty
-- ['z']
--
-- >>> 9 +: 'z' +: empty
-- [9,'z']
class (f ~ SeparatedConsF g, g ~ SeparatedConsG f) => SeparatedCons f g where
  type SeparatedConsF g :: * -> * -> *
  type SeparatedConsG f :: * -> * -> *
  (+:) ::
    a
    -> f s a
    -> g a s

instance SeparatedCons Separated Separated1 where
  type SeparatedConsF Separated1 = Separated
  type SeparatedConsG Separated = Separated1
  a +: Separated x =
    Separated1 a x

instance SeparatedCons Separated1 Separated where
  type SeparatedConsF Separated = Separated1
  type SeparatedConsG Separated1 = Separated
  s +: Separated1 a x =
    Separated ((s, a) : x)

infixr 5 +:

-- | Append two lists of separated values to produce a list of pairs of separator and element values.
--
-- >>> single 7 ++: single 'a'
-- [7,'a']
--
-- 'a' +: single 7 ++: single 'b'
-- ['a',7,'b']
--
-- prop> a +: (b ++: c) == (a +: b) *+: c
(++:) ::
   Separated1 s a
   -> Separated1 a s
   -> Separated s a
Separated1 s x ++: Separated1 t y =
  let (q, r') = (s, x) ^. separated1Iso . shift
  in Separated (q <> ((r', t) : y))

infixr 5 ++:

-- | Append element values interspersed with a separator to a list of pairs of separator and element values.
--
-- >>> empty *+: single 7
-- [7]
--
-- >>> empty *+: 6 +: 'x' +: single 7
-- [6,'x',7]
--
-- >>> 'w' +: empty *+: 6 +: 'x' +: single 7
-- ['w',6,'x',7]
(*+:) ::
  Separated s a
  -> Separated1 s a
  -> Separated1 s a
Separated x *+: Separated1 t y =
  let (z, w') = separated1Iso . shift # (x, t)
  in Separated1 z (w' <> y)

infixr 5 *+:

-- | Append a list of pairs of separator and element values to element values interspersed with a separator.
--
-- >>> single 7 **: empty
-- [7]
--
-- >>> single 6 **: 'x' +: 7 +: empty
-- [6,'x',7]
--
-- >>> 'w' +: single 6 **: 'x' +: 7 +: empty
-- ['w',6,'x',7]
(**:) ::
  Separated1 a s
  -> Separated s a
  -> Separated1 a s
Separated1 a x **: Separated y =
  Separated1 a (x <> y)

infixr 5 **:

-- | An empty list of pairs of separator and element values.
--
-- >>> empty
-- []
--
-- prop> empty *+: x == x
--
-- prop> x **: empty == x
empty ::
  Separated a s
empty =
  Separated []

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
  Separated1 a []

-- | One element and one separator.
--
-- >>> 7 +:. "abc"
-- [7,"abc"]
--
-- >>> 7 +: "abc" +: 8 +:. "def"
-- [7,"abc",8,"def"]
(+:.) ::
  a
  -> s
  -> Separated a s
(+:.) a s =
  a +: single s

infixr 5 +:.

-- | Return all element values in a list of pairs of element and separator values.
--
-- separatedValues empty
-- []
--
-- separatedValues ('x' +: 2 +: empty)
-- [2]
separatedValues ::
  Separated a s
  -> [a]
separatedValues (Separated x) =
  fmap fst x

-- | Return all element values.
--
-- >>> separated1Values (single 8)
-- 8 :| []
--
-- >>> separated1Values (7 +: 'a' +: single 8)
-- 7 :| [8]
--
-- prop> let h :| _ = separated1Values (single x) in h == (x :: Int)
--
-- prop> let _ :| t = separated1Values (d +: e +: single x) in t == fmap fst [e]
separated1Values ::
  Separated1 a s
  -> NonEmpty a
separated1Values (Separated1 a x) =
  a :| fmap snd x

-- | Return all separator values.
--
-- >>> separators empty
-- []
--
-- separators ('x' +: 2 +: empty)
-- ['x']
separators ::
  Separated s a
  -> [s]
separators (Separated x) =
  fmap fst x

-- | Return all separator values.
--
-- >>> separators ('a' +: single 7)
-- "a"
--
-- >>> separators ('a' +: 6 +:'b' +: single 7)
-- "ab"
--
-- prop> separators (a +: single x) == [a]
separators1 ::
  Separated1 a s
  -> [s]
separators1 (Separated1 _ x) =
  fmap fst x

-- | Extract all values, where the separator and element are the same type.
--
-- >>> allValues empty
-- []
--
-- >>> allValues (1 +: 2 +: 3 +: 4 +: empty)
-- [1,2,3,4]
allValues ::
  Separated' a
  -> [a]
allValues (Separated x) =
  x >>= \(s, a') -> [s, a']

-- | Extract all values, where the separator and element are the same type.
--
-- >>> allValues1 (single 7)
-- 7 :| []
--
-- >>> allValues1 (1 +: 2 +: 3 +: empty)
-- 1 :| [2,3]
allValues1 ::
  Separated1' a
  -> NonEmpty a
allValues1 (Separated1 a x) =
  a :| (x >>= \(s, a') -> [s, a'])

-- | The isomorphism to a list of pairs of element and separator values.
--
-- >>> separatedIso # empty
-- []
--
-- >>> separatedIso # ('x' +: 6 +: empty)
-- [('x',6)]
--
-- >>> [] ^. separatedIso
-- []
--
-- [(6, [])] ^. separatedIso
-- [6,[]]
separatedIso ::
  Iso' [(s, a)] (Separated s a)
separatedIso =
  iso Separated (\(Separated x) -> x)

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
  Iso' (Separated s a) (Separated a s)
separatedSwap =
  let swap (a, b) = (b, a)
  in iso (\(Separated x) -> Separated (fmap swap x)) (\(Separated x) -> Separated (fmap swap x))

-- | The isomorphism to element values interspersed with a separator.
--
-- >>> separated1Iso # (single 6)
-- (6,[])
--
-- >>> separated1Iso # (5 +: 'x' +: single 6)
-- (5,[('x',6)])
--
-- >>> (6, []) ^. separated1Iso
-- [6]
--
-- >>> (5, [('x', 6)]) ^. separated1Iso
-- [5,'x',6]
separated1Iso ::
  Iso' (a, [(s, a)]) (Separated1 a s)
separated1Iso =
  iso (\(a, x) -> Separated1 a x) (\(Separated1 a x) -> (a, x))

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
  Iso' (Separated1 a s) ([(a, s)], a)
shift =
  let shiftR ([], a) =
        Separated1 a []
      shiftR ((b, s):r, a) =
        let Separated1 z' w = shiftR (r, b)
        in Separated1 z' ((s, a) : w)
      shiftL (Separated1 s' []) =
        ([], s')
      shiftL (Separated1 s' ((a, t') : r)) =
        let (w, z) = shiftL (Separated1 t' r)
        in ((s', a) : w, z)
  in iso shiftL shiftR

-- | A lens on the first element value.
--
-- >>> single 7 ^. separated1Head
-- 7
--
-- prop> single x ^. separated1Head == (x :: Int)
separated1Head ::
  Lens' (Separated1 a s) a
separated1Head =
  lens (\(Separated1 a _) -> a) (\(Separated1 _ x) a -> Separated1 a x)

-- | A lens on the tail.
--
-- prop> d +: e +: single x ^. separated1Tail == e +: x +: empty
separated1Tail ::
  Lens' (Separated1 a s) (Separated s a)
separated1Tail =
  lens (\(Separated1 _ x) -> Separated x) (\(Separated1 a _) (Separated x) -> Separated1 a x)

-- | Effectful separation with failure represented by @Nothing@.
--
-- >>> separatedWith Nothing Nothing
-- Just Nothing
--
-- >>> separatedWith Nothing (Just 7)
-- Just Nothing
--
-- >>> separatedWith (Just 'x') Nothing
-- Just (Just ['x'])
--
-- >>> separatedWith [] []
-- [Nothing]
--
-- >>> separatedWith [] [1,2,3]
-- [Nothing]
--
-- >>> separatedWith [1,2,3] []
-- [Just [1],Just [2],Just [3],Nothing]
separatedWith ::
  Alternative f =>
  f s
  -> f a
  -> f (Maybe (Separated1 s a))
separatedWith a s =
  Just <$> separatedWith1 a s <|> pure Nothing

-- | Effectful separation.
--
-- >>> separatedWith1 Nothing Nothing
-- Nothing
--
-- >>> separatedWith1 Nothing (Just 7)
-- Nothing
--
-- >>> separatedWith1 (Just 'x') Nothing
-- Just ['x']
--
-- >>> separatedWith1 [] []
-- []
--
-- >>> separatedWith1 [] [1,2,3]
-- []
--
-- >>> separatedWith1 [1,2,3] []
-- [[1],[2],[3]]
separatedWith1 ::
  Alternative f =>
  f a
  -> f s
  -> f (Separated1 a s)
separatedWith1 a s =
  Separated1 <$> a <*> many ((,) <$> s <*> a)

data Separateds1 a s =
  Separateds1 [Separated s a] (Separated1 a s)
  deriving (Eq, Ord, Show)

instance HasSeparated1 Separateds1 where
  separated1L =
    lens (\(Separateds1 _ x) -> x) (\(Separateds1 q _) x -> Separateds1 q x)

class HasManySeparated f where
  manyseparatedL ::
    Lens' (f a s) [Separated s a]

instance HasManySeparated Separateds1 where
  manyseparatedL =
    lens (\(Separateds1 q _) -> q) (\(Separateds1 _ x) q -> Separateds1 q x)

delineate ::
  Separateds1 a s
  -> Separateds1 a s
delineate (Separateds1 a z) =
  Separateds1 (empty : a) z

data Separateds s a =
  Separateds s [Separated s a] (Separated1 a s)
  deriving (Eq, Ord, Show)

instance HasFSeparated1 Separateds where
  fseparated1L =
    lens (\(Separateds _ _ x) -> x) (\(Separateds a q _) x -> Separateds a q x)

class HasFManySeparated f where
  fmanySeparatedL ::
    Lens' (f s a) [Separated s a]

instance HasFManySeparated Separateds where
  fmanySeparatedL =
    lens (\(Separateds _ q _) -> q) (\(Separateds a _ x) q -> Separateds a q x)

firstSeparator ::
  Lens' (Separateds s a) s
firstSeparator =
  lens (\(Separateds a _ _) -> a) (\(Separateds _ q x) a -> Separateds a q x)

instance SeparatedCons Separateds Separateds1 where
  type SeparatedConsF Separateds1 = Separateds
  type SeparatedConsG Separateds = Separateds1
  s +: Separateds a q x =
    Separateds1 ((a +:. s) : q) x

instance SeparatedCons Separateds1 Separateds where
  type SeparatedConsF Separateds = Separateds1
  type SeparatedConsG Separateds1 = Separateds
  s +: Separateds1 q x =
    Separateds s q x
