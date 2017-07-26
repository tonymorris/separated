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
, (.++.)
, (++.)
, (.++)
, separatedBy
, separatedBy1
) where

import Control.Applicative(Alternative(many))
import Data.Bifoldable
import Data.Bitraversable
import Data.Functor.Apply as Apply((<.>))
import Data.List(intercalate, zipWith, repeat)
import Data.Monoid as Monoid(mappend)
import Data.Semigroup as Semigroup((<>))
import Data.Separated.SeparatedCons(SeparatedCons((+:), SeparatedConsF, SeparatedConsG))
import Data.String(String)
import Papa hiding ((<.>), empty, repeat)

-- $setup
-- >>> :set -XNoImplicitPrelude
-- >>> import Data.Char(toUpper)
-- >>> import Data.Either(isLeft)
-- >>> import Text.Parsec(parse, char, digit)
-- >>> import Test.QuickCheck(Arbitrary(..))
-- >>> instance (Arbitrary s, Arbitrary a) => Arbitrary (Separated s a) where arbitrary = fmap (^. separated) arbitrary
-- >>> instance (Arbitrary a, Arbitrary s) => Arbitrary (Separated1 s a) where arbitrary = do a <- arbitrary; x <- arbitrary; return ((a, x) ^. separated1)


newtype Separated a b =
  Separated [(a, b)]
  deriving (Eq, Ord)

instance Bifunctor Separated where
  bimap f g (Separated x) =
    Separated (fmap (bimap f g) x)

instance Bifoldable Separated where
  bifoldr f g z (Separated x) =
    foldr (\(a, b) -> f a . g b) z x

instance Bitraversable Separated where
  bitraverse f g (Separated x) =
    Separated <$> traverse (\(a, b) -> (,) <$> f a <*> g b) x

-- | Map across a @Separated@ on the element values.
--
-- prop> fmap id (x :: Separated Int String) == x
--
-- prop> \a b -> fmap (+1) (a +: b +: empty) == a +: (1+b) +: empty
instance Functor (Separated a) where
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
instance Semigroup a => Apply (Separated a) where
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
instance (Semigroup a, Monoid a) => Applicative (Separated a) where    
  (<*>) =
    separatedAp (<>)
  pure =
    Separated . repeat . (,) mempty

instance (Show a, Show b) => Show (Separated a b) where
  show (Separated x) =
    showSeparated id x

instance Semigroup (Separated a b) where
  Separated x <> Separated y =
    Separated (x <> y)    

instance Monoid (Separated a b) where
  mappend =
    (<>)
  mempty =
    Separated mempty

instance SeparatedCons Separated1 Separated where
  type SeparatedConsF Separated = Separated1
  type SeparatedConsG Separated1 = Separated
  s +: Separated1 a (Separated x) =
    Separated ((s, a) : x)

----

data Separated1 b a =
  Separated1 b (Separated a b)
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
instance Functor (Separated1 b) where
  fmap =
    bimap id

-- | Applies functions with separator values, using a zipping operation,
-- appending elements.
--
-- >>> [1,2] +: reverse +: [3,4] +: empty <.> [5,6,7] +: "abc" +: [8] +: empty
-- [[1,2,5,6,7],"cba",[3,4,8]]
instance Semigroup b => Apply (Separated1 b) where
  (<.>) =
    separated1Ap (<>)

instance (Show b, Show a) => Show (Separated1 b a) where
  show (Separated1 a (Separated x)) =
    showSeparated (show a:) x
    
-- | Applies functions with separator values, using a zipping operation,
-- appending elements. The identity operation is an infinite list of the empty
-- element and the given separator value.
--
-- >>> [1,2] +: reverse +: [3,4] +: empty <*> [5,6,7] +: "abc" +: [8] +: empty
-- [[1,2,5,6,7],"cba",[3,4,8]]
instance (Semigroup b, Monoid b) => Applicative (Separated1 b) where    
  (<*>) =
    separated1Ap (<>)
  pure =
    Separated1 mempty . (separatedSwap #) . pure

instance SeparatedCons Separated Separated1 where
  type SeparatedConsF Separated1 = Separated
  type SeparatedConsG Separated = Separated1
  (+:) =
    Separated1

----

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
  Iso [(a, b)] [(c, d)] (Separated a b) (Separated c d)
separated =
  iso Separated (\(Separated x) -> x)

empty ::
  Separated s a
empty =
  Separated []

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
  iso (uncurry Separated1) (\(Separated1 a x) -> (a, x))

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

infixl 9 +-

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

-- | Append two lists of separated values to produce a list of pairs of separator and element values.
--
-- >>> single 7 .++. single 'a'
-- [7,'a']
--
-- 'a' +: single 7 .++. single 'b'
-- ['a',7,'b']
--
-- prop> a +: (b :: Separated Int Int) == a +: b --  (a +: (b .++. c)) == ((a +: b) .++ c)
(.++.) ::
   Separated1 s a
   -> Separated1 a s
   -> Separated s a
Separated1 s x .++. Separated1 t (Separated y) =
  let (q, r') = (s, x) ^. separated1 . shift
  in Separated (q <> ((r', t) : y)) 

infixr 5 .++.

-- | Append element values interspersed with a separator to a list of pairs of separator and element values.
--
-- >>> empty ++. single 7
-- [7]
--
-- >>> empty ++. 6 +: 'x' +: single 7
-- [6,'x',7]
--
-- >>> 'w' +: empty ++. 6 +: 'x' +: single 7
-- ['w',6,'x',7]
(++.) ::
  Separated s a
  -> Separated1 s a
  -> Separated1 s a
Separated x ++. Separated1 t y =
  let (z, w') = separated1 . shift # (x, t)
  in Separated1 z (w' <> y)

infixr 5 ++.

-- | Append a list of pairs of separator and element values to element values interspersed with a separator.
--
-- >>> single 7 .++ empty
-- [7]
--
-- >>> single 6 .++ 'x' +: 7 +: empty
-- [6,'x',7]
--
-- >>> 'w' +: single 6 .++ 'x' +: 7 +: empty
-- ['w',6,'x',7]
(.++) ::
  Separated1 a s
  -> Separated s a
  -> Separated1 a s
Separated1 a x .++ y =
  Separated1 a (x <> y)

infixr 5 .++

-- |
--
-- >>> parse (separatedBy (char ',') digit) "test" ""
-- Right []
--
-- >>> isLeft (parse (separatedBy (char ',') digit) "test" ",")
-- True
--
-- >>> parse (separatedBy (char ',') digit) "test" ",1"
-- Right [',','1']
--
-- >>> isLeft (parse (separatedBy (char ',') digit) "test" ",1,")
-- True
--
-- >>> parse (separatedBy (char ',') digit) "test" ",1,2,3,4,5"
-- Right [',','1',',','2',',','3',',','4',',','5']
separatedBy ::
  Alternative f =>
  f a
  -> f b
  -> f (Separated a b)
separatedBy a b =
  Separated <$>
    many
      ((,) <$> a <*> b)

-- |
--
-- >>> isLeft (parse (separatedBy1 (char ',') digit) "test" "")
-- True
--
-- >>> parse (separatedBy1 (char ',') digit) "test" ","
-- Right [',']
--
-- >>> isLeft (parse (separatedBy1 (char ',') digit) "test" ",1")
-- True
--
-- >>> parse (separatedBy1 (char ',') digit) "test" ",1,"
-- Right [',','1',',']
--
-- >>>  parse (separatedBy1 (char ',') digit) "test" ",1,2,3,4,5,"
-- Right [',','1',',','2',',','3',',','4',',','5',',']
separatedBy1 ::
  Alternative f =>
  f b
  -> f a
  -> f (Separated1 b a)
separatedBy1 b a =
  Separated1 <$> b <*> separatedBy a b

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
separatedAp opp (Separated f) (Separated a) =
    Separated (zipWith (\(s, f') (t, a') -> (s `opp` t, f' a')) f a)  

separated1Ap ::
  (a -> a -> a)
  -> Separated1 a (s -> t)
  -> Separated1 a s
  -> Separated1 a t
separated1Ap opp (Separated1 f (Separated fs)) (Separated1 a (Separated as)) =
    Separated1 (f `opp` a) (Separated (zipWith (\(s, f') (t, a') -> (s t, f' `opp` a')) fs as))
