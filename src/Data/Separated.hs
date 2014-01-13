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
  -- * Extracting values from data types
, allValues
, allValues1
, separatedValues
, separated1Values
, separators
, separators1
  -- * Lenses and isomorphisms
, separatedIso
, separatedSwap
, separated1Iso
, shift
, separated1Head
, separated1Tail
  -- * Alternating combinators
, separatedWith
, separatedWith1
  -- * Zipper
, SepZipper
, SepZipper'
, Sep1Zipper
, Sep1Zipper'
  -- ** construction
, zipper
  -- ** running a potential zipper as if it were
, Possible(..)
  -- ** move focus
, Move(..)
, HasAdjacent(..)
, FindAdjacent(..)
  -- ** modify focus
, withfocus
, withfocus1
, setfocus
, (.=)
, setfocus1
, (.=.)
  -- ** zipper components
, focus
, focus1
, lefts
, lefts1
, rights
, rights1
) where

import Prelude(Eq, Ord, Show(..), Functor(..), Monad(..), Bool(..), fst, snd, const, id, not, (.))
import Data.List.NonEmpty(NonEmpty(..))
import Data.List(intercalate, zipWith, repeat, null)
import Control.Lens(Lens', Iso', lens, iso, (#), (^.))
import Data.Semigroup(Semigroup(..))
import Data.Monoid(Monoid(..))
import Data.Functor((<$>))
import Data.Maybe(Maybe(..))
import Data.Either(Either(..), either)
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
instance Monoid s => Applicative (Separated1 s) where
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

-- | Zero element values interspersed with one separator.
--
-- >>> single 4
-- [4]
--
-- prop> single x ^. separated1Tail == empty
single ::
  s
  -> Separated1 s a
single a =
  Separated1 a []

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

-- | Construct a zipper with the focus set to the first element value and
-- subsequent elements and separators to the right.
--
-- >>> zipper (1 +: empty)
-- [] >1< []
--
-- zipper (2 +: 'x' +: 1 +: empty)
-- [] >2< ['x',1]
zipper ::
  Separated1 a s
  -> SepZipper Z s a
zipper (Separated1 a x) =
  SepZ [] a x

data Z
data MaybeZ

data SepZipper z s a where
  SepZ ::
    [(s, a)]
    -> a
    -> [(s, a)]
    -> SepZipper Z s a
  SepMaybeZ ::
    Maybe (SepZipper Z s a)
    -> SepZipper MaybeZ s a
deriving instance (Eq s, Eq a) => Eq (SepZipper z s a)
deriving instance (Ord s, Ord a) => Ord (SepZipper z s a)

type SepZipper' z x =
  SepZipper z x x

instance (Show s, Show a) => Show (SepZipper z s a) where
  show (SepZ l x r) =
    let sh q = '[' : intercalate "," (fmap (\(s, a) -> show s <> "," <> show a) q) <> "]"
    in sh l <> " >" <> show x <> "< " <> sh r
  show (SepMaybeZ Nothing) =
    "><"
  show (SepMaybeZ (Just z)) =
    show z

-- | Map on element values.
--
-- >>> fmap (+3) (zipper (2 +: 'x' +: 1 +: empty))
-- [] >5< ['x',4]
--
-- >>> fmap (+3) (moveleft (zipper (2 +: 'x' +: 1 +: empty)))
-- ><
instance Functor (SepZipper z s) where
  fmap f (SepZ l x r) =
    SepZ (fmap (\(a, b) -> (a, f b)) l) (f x) (fmap (\(a, b) -> (a, f b)) r)
  fmap f (SepMaybeZ z) =
    SepMaybeZ (fmap (fmap f) z)

data Sep1Zipper z s a where
  Sep1Z ::
    [(s, a)]
    -> a
    -> s
    -> a
    -> [(s, a)]
    -> Sep1Zipper Z s a
  Sep1MaybeZ ::
    Maybe (Sep1Zipper Z s a)
    -> Sep1Zipper MaybeZ s a
deriving instance (Eq s, Eq a) => Eq (Sep1Zipper z s a)
deriving instance (Ord s, Ord a) => Ord (Sep1Zipper z s a)

type Sep1Zipper' z x =
  Sep1Zipper z x x

instance (Show s, Show a) => Show (Sep1Zipper z s a) where
  show (Sep1Z l lx x rx r) =
    let sh pre q = '[' : intercalate "," (pre (fmap (\(s, a) -> show s <> "," <> show a) q)) <> "]"
    in sh (show lx :) l <> " >" <> show x <> "< " <> sh (show rx :) r
  show (Sep1MaybeZ Nothing) =
    "><"
  show (Sep1MaybeZ (Just z)) =
    show z

-- | Map on element values.
--
-- >>> fmap (+3) (moveright (zipper (2 +: 'x' +: 1 +: empty)))
-- [5] >'x'< [4]
instance Functor (Sep1Zipper z s) where
  fmap f (Sep1Z l la x ra r) =
    Sep1Z (fmap (\(a, b) -> (a, f b)) l) (f la) x (f ra) (fmap (\(a, b) -> (a, f b)) r)
  fmap f (Sep1MaybeZ z) =
    Sep1MaybeZ (fmap (fmap f) z)

class Possible z where
  (?.) ::
    (z Z s a -> x)
    -> z MaybeZ s a
    -> Maybe x
  (.?.) ::
    (z Z s a -> z Z s a)
    -> z MaybeZ s a
    -> z MaybeZ s a

instance Possible SepZipper where
  f ?. SepMaybeZ z =
    fmap f z
  f .?. z =
    SepMaybeZ (f ?. z)

instance Possible Sep1Zipper where
  f ?. Sep1MaybeZ z =
    fmap f z
  f .?. z =
    Sep1MaybeZ (f ?. z)

class (f ~ MoveF g, g ~ MoveG f) => Move f g where
  type MoveF g :: * -> * -> * -> *
  type MoveG f :: * -> * -> * -> *
  moveright ::
    f z s a
    -> g MaybeZ s a
  moveleft ::
    f z s a
    -> g MaybeZ s a

instance Move SepZipper Sep1Zipper where
  type MoveF Sep1Zipper = SepZipper
  type MoveG SepZipper = Sep1Zipper
  moveright (SepZ _ _ []) =
    Sep1MaybeZ Nothing
  moveright (SepZ l x ((s,a):t)) =
    Sep1MaybeZ (Just (Sep1Z l x s a t))
  moveright (SepMaybeZ Nothing) =
    Sep1MaybeZ Nothing
  moveright (SepMaybeZ (Just z)) =
    moveright z
  moveleft (SepZ [] _ _) =
    Sep1MaybeZ Nothing
  moveleft (SepZ ((s,a):l) x r) =
    Sep1MaybeZ (Just (Sep1Z l a s x r))
  moveleft (SepMaybeZ Nothing) =
    Sep1MaybeZ Nothing
  moveleft (SepMaybeZ (Just z)) =
    moveleft z

instance Move Sep1Zipper SepZipper where
  type MoveF SepZipper = Sep1Zipper
  type MoveG Sep1Zipper = SepZipper
  moveright (Sep1Z l la x ra r) =
    SepMaybeZ (Just (SepZ ((x,la):l) ra r))
  moveright (Sep1MaybeZ Nothing) =
    SepMaybeZ Nothing
  moveright (Sep1MaybeZ (Just z)) =
    moveright z
  moveleft (Sep1Z l la x ra r) =
    SepMaybeZ (Just (SepZ l la ((x,ra):r)))
  moveleft (Sep1MaybeZ Nothing) =
    SepMaybeZ Nothing
  moveleft (Sep1MaybeZ (Just z)) =
    moveleft z

-- | Modify the current focus with the given operation.
--
-- >>> withfocus (+10) (zipper (2 +: 'x' +: 1 +: empty))
-- [] >12< ['x',1]
withfocus ::
  (a -> a)
  -> SepZipper z s a
  -> SepZipper z s a
withfocus k (SepZ l a r) =
  SepZ l (k a) r
withfocus k (SepMaybeZ z) =
  SepMaybeZ (fmap (withfocus k) z)

-- | Modify the current focus with the given operation.
--
-- >>> withfocus1 (+10) (moveright (zipper ('w' +: 1 +: 'x' +: empty)))
-- ['w'] >11< ['x']
withfocus1 ::
  (s -> s)
  -> Sep1Zipper z s a
  -> Sep1Zipper z s a
withfocus1 k (Sep1Z l la s ra r) =
  Sep1Z l la (k s) ra r
withfocus1 k (Sep1MaybeZ z) =
  Sep1MaybeZ (fmap (withfocus1 k) z)

-- | Modify the current focus to the given value.
--
-- >>> setfocus 13 (zipper (2 +: 'x' +: 1 +: empty))
-- [] >13< ['x',1]
setfocus ::
  a
  -> SepZipper z s a
  -> SepZipper z s a
setfocus =
  withfocus . const

-- | Modify the current focus to the given value.
--
-- >>> zipper (2 +: 'x' +: 1 +: empty) .= 13
-- [] >13< ['x',1]
(.=) ::
  SepZipper z s a
  -> a
  -> SepZipper z s a
z .= a =
  setfocus a z

-- | Modify the current focus to the given value.
--
-- >>> setfocus1 3 (moveright (zipper ('w' +: 1 +: 'x' +: empty)))
-- ['w'] >3< ['x']
setfocus1 ::
  s
  -> Sep1Zipper z s a
  -> Sep1Zipper z s a
setfocus1 =
  withfocus1 . const

-- | Modify the current focus to the given value.
--
-- >>> moveright (zipper ('w' +: 1 +: 'x' +: empty)) .=. 3
-- ['w'] >3< ['x']
(.=.) ::
  Sep1Zipper z s a
  -> s
  -> Sep1Zipper z s a
z .=. a =
  setfocus1 a z

-- | A lens on a zipper focus.
--
-- >>> set focus 14 (zipper (1 +: empty))
-- [] >14< []
--
-- >>> zipper (1 +: empty) ^. focus
-- 1
--
-- prop> zipper (x +: empty) ^. focus == x
focus ::
  Lens' (SepZipper Z s a) a
focus =
  lens (\(SepZ _ x _) -> x) (\(SepZ l _ r) x -> SepZ l x r)

-- | A lens on a zipper focus.
focus1 ::
  Lens' (Sep1Zipper Z s a) s
focus1 =
  lens (\(Sep1Z _ _ x _ _) -> x) (\(Sep1Z l la _ ra r) x -> Sep1Z l la x ra r)

lefts ::
  Lens' (SepZipper Z s a) (Separated s a)
lefts =
  lens (\(SepZ l _ _) -> Separated l) (\(SepZ _ x r) (Separated l) -> SepZ l x r)

lefts1 ::
  Lens' (Sep1Zipper Z s a) (Separated1 a s)
lefts1 =
  lens (\(Sep1Z l la _ _ _) -> Separated1 la l) (\(Sep1Z _ _ x ra r) (Separated1 la l) -> Sep1Z l la x ra r)

rights ::
  Lens' (SepZipper Z s a) (Separated s a)
rights =
  lens (\(SepZ _ _ r) -> Separated r) (\(SepZ l x _) (Separated r) -> SepZ l x r)

rights1 ::
  Lens' (Sep1Zipper Z s a) (Separated1 a s)
rights1 =
  lens (\(Sep1Z _ _ _ ra r) -> Separated1 ra r) (\(Sep1Z l la x _ _) (Separated1 ra r) -> Sep1Z l la x ra r)

class HasAdjacent z p where
  type HasAdjacentZ z :: * -> * -> * -> *
  type HasAdjacentP p :: *
  hasleft ::
    z p s a
    -> Bool
  hasright ::
    z p s a
    -> Bool

instance HasAdjacent SepZipper Z where
  type HasAdjacentZ SepZipper = SepZipper
  type HasAdjacentP Z = Z
  hasleft (SepZ l _ _) =
    not (null l)
  hasright (SepZ _ _ r) =
    not (null r)

instance HasAdjacent Sep1Zipper Z where
  type HasAdjacentZ Sep1Zipper = Sep1Zipper
  type HasAdjacentP Z = Z
  hasleft _ =
    True
  hasright _ =
    True

instance HasAdjacent SepZipper MaybeZ where
  type HasAdjacentZ SepZipper = SepZipper
  type HasAdjacentP MaybeZ = MaybeZ
  hasleft (SepMaybeZ (Just z)) =
    hasleft z
  hasleft (SepMaybeZ Nothing) =
    False
  hasright (SepMaybeZ (Just z)) =
    hasright z
  hasright (SepMaybeZ Nothing) =
    False

instance HasAdjacent Sep1Zipper MaybeZ where
  type HasAdjacentZ Sep1Zipper = Sep1Zipper
  type HasAdjacentP MaybeZ = MaybeZ
  hasleft (Sep1MaybeZ (Just _)) =
    True
  hasleft (Sep1MaybeZ Nothing) =
    False
  hasright (Sep1MaybeZ (Just _)) =
    True
  hasright (Sep1MaybeZ Nothing) =
    False

class (f ~ FindAdjacentF g, g ~ FindAdjacentG f) => FindAdjacent f g p where
  type FindAdjacentF g :: * -> * -> * -> *
  type FindAdjacentG f :: * -> * -> * -> *
  type FindAdjacentP p :: *
  findleft ::
    (Either a s -> Bool)
    -> f p s a
    -> Maybe (Either (f p s a) (g p s a))
  findright ::
    (Either a s -> Bool)
    -> f p s a
    -> Maybe (Either (f p s a) (g p s a))

instance FindAdjacent SepZipper Sep1Zipper Z where
  type FindAdjacentF Sep1Zipper = SepZipper
  type FindAdjacentG SepZipper = Sep1Zipper
  type FindAdjacentP Z = Z
  findleft p z =
    let Sep1MaybeZ r = moveleft z
    in r >>= \r' ->
         if p (Right (r' ^. focus1))
           then Just (Right r')
           else fmap (either Right Left) (findleft p r')
  findright p z =
    let Sep1MaybeZ r = moveright z
    in r >>= \r' ->
         if p (Right (r' ^. focus1))
           then Just (Right r')
           else fmap (either Right Left) (findright p r')

instance FindAdjacent Sep1Zipper SepZipper Z where
  type FindAdjacentF SepZipper = Sep1Zipper
  type FindAdjacentG Sep1Zipper = SepZipper
  type FindAdjacentP Z = Z
  findleft p z =
    let SepMaybeZ r = moveleft z
    in r >>= \r' ->
         if p (Left (r' ^. focus))
           then Just (Right r')
           else fmap (either Right Left) (findleft p r')
  findright p z =
    let SepMaybeZ r = moveright z
    in r >>= \r' ->
         if p (Left (r' ^. focus))
           then Just (Right r')
           else fmap (either Right Left) (findright p r')
{-
todo

* tests

 -}

