{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Separated.FlipSeparated(
  FlipSeparated
, flipSeparated
, flipSeparated1
, fempty
) where

import Control.Applicative(Applicative(pure, (<*>)))
import Control.Category(Category(id, (.)))
import Control.Lens.Getter((^.))
import Control.Lens.Iso(Iso, iso)
import Control.Lens.Review((#))
import Data.Bifunctor(Bifunctor(bimap))
import Data.Eq(Eq)
import Data.Functor(Functor(fmap))
import Data.Functor.Apply(Apply((<.>)))
import Data.List(zipWith)
import Data.Monoid(Monoid(mappend, mempty))
import Data.Ord(Ord)
import Data.Semigroup(Semigroup((<>)))
import Data.Separated.FlipSeparatedCons(FlipSeparatedCons(FlipSeparatedConsF, FlipSeparatedConsG, (+.)))
import Data.Separated.Separated(Separated, Separated1, separated, separated1, separatedSwap, empty)
import Data.Separated.SeparatedCons((+:))
import Prelude(Show(show))

-- $setup
-- >>> :set -XNoImplicitPrelude
-- >>> import Control.Monad(Monad(return))
-- >>> import Data.Int(Int)
-- >>> import Data.Eq(Eq((==)))
-- >>> import Data.Separated.Separated(empty, single)
-- >>> import Data.String(String)
-- >>> import Prelude(Num((+)))
-- >>> import Test.QuickCheck(Arbitrary(..))
-- >>> instance (Arbitrary s, Arbitrary a) => Arbitrary (Separated s a) where arbitrary = fmap (^. separated) arbitrary
-- >>> instance (Arbitrary a, Arbitrary s) => Arbitrary (Separated1 s a) where arbitrary = do a <- arbitrary; x <- arbitrary; return ((a, x) ^. separated1)
-- >>> instance (Arbitrary s, Arbitrary a) => Arbitrary (FlipSeparated a s) where arbitrary = fmap FlipSeparated arbitrary
-- >>> instance (Arbitrary a, Arbitrary s) => Arbitrary (FlipSeparated1 s a) where arbitrary = do a <- arbitrary; return (FlipSeparated1 a)

newtype FlipSeparated a s =
  FlipSeparated (Separated s a)
  deriving (Eq, Ord)

instance Bifunctor FlipSeparated where
  bimap f g (FlipSeparated x) =
    FlipSeparated (bimap g f x)

-- | Map across a @FlipSeparated@ on the separator values.
--
-- prop> fmap id (x :: FlipSeparated Int String) == x
--
-- prop> fmap (+1) (a +. b +. fempty) == (1+a) +. b +. fempty
instance Functor (FlipSeparated a) where
  fmap =
    bimap id

instance Semigroup a => Apply (FlipSeparated a) where
  FlipSeparated x <.> FlipSeparated y =
    FlipSeparated (separatedSwap # (x ^. separatedSwap <.> y ^. separatedSwap))

instance Monoid s => Applicative (FlipSeparated s) where    
  FlipSeparated x <*> FlipSeparated y =
    FlipSeparated (separatedSwap # (x ^. separatedSwap <*> y ^. separatedSwap))
  pure =
    FlipSeparated . (#) separatedSwap . pure

instance (Show s, Show a) => Show (FlipSeparated s a) where
  show (FlipSeparated x) =
    show x

instance Semigroup (FlipSeparated s a) where
  FlipSeparated x <> FlipSeparated y =
    FlipSeparated (x <> y)    

instance Monoid (FlipSeparated s a) where
  mappend =
    (<>)
  mempty =
    FlipSeparated mempty

instance FlipSeparatedCons FlipSeparated1 FlipSeparated where
  type FlipSeparatedConsF FlipSeparated = FlipSeparated1
  type FlipSeparatedConsG FlipSeparated1 = FlipSeparated
  s +. p =
    (s +: flipSeparated1 # p) ^. flipSeparated

-- | The isomorphism to a @Separator@.
--
-- >>> empty ^. flipSeparated
-- []
--
-- >>> ('x' +: 6 +: empty) ^. flipSeparated
-- ['x',6]
--
-- >>> [] ^. separated . flipSeparated
-- []
--
-- >>> [(6, [])] ^. separated . flipSeparated
-- [6,[]]
flipSeparated ::
  Iso (Separated s a) (Separated t b) (FlipSeparated a s) (FlipSeparated b t) 
flipSeparated =
  iso FlipSeparated (\(FlipSeparated x) -> x) 

fempty ::
  FlipSeparated a s
fempty =
  FlipSeparated empty

newtype FlipSeparated1 s a =
  FlipSeparated1 (Separated1 a s)

instance Bifunctor FlipSeparated1 where
  bimap f g (FlipSeparated1 x) =
    FlipSeparated1 (bimap g f x)

instance Functor (FlipSeparated1 a) where
  fmap =
    bimap id

instance Semigroup a => Apply (FlipSeparated1 a) where
  (<.>) =
    flipSeparated1Ap (<>)

instance Monoid s => Applicative (FlipSeparated1 s) where    
  (<*>) =
    flipSeparated1Ap mappend
  pure a =
    FlipSeparated1 ((a, pure a) ^. separated1)

instance (Show s, Show a) => Show (FlipSeparated1 s a) where
  show (FlipSeparated1 x) =
    show x

-- | The isomorphism to a @Separated1@.
--
-- >>>  single 6 ^. flipSeparated1
-- [6]
--
-- >>>  (5 +: 'x' +: single 6) ^. flipSeparated1
-- [5,'x',6]
--
-- >>> (6 +: empty) ^. flipSeparated1
-- [6]
--
-- >>> (5 +: 'x' +: 6 +: empty) ^. flipSeparated1
-- [5,'x',6]
flipSeparated1 ::
  Iso (Separated1 a s) (Separated1 b t) (FlipSeparated1 s a) (FlipSeparated1 t b)
flipSeparated1 =
  iso FlipSeparated1 (\(FlipSeparated1 x) -> x)

instance FlipSeparatedCons FlipSeparated FlipSeparated1 where
  type FlipSeparatedConsF FlipSeparated1 = FlipSeparated
  type FlipSeparatedConsG FlipSeparated = FlipSeparated1
  a +. p =
    (a +: flipSeparated # p) ^. flipSeparated1

----

flipSeparated1Ap ::
  (s -> s -> s)
  -> FlipSeparated1 s (a -> b)
  -> FlipSeparated1 s a
  -> FlipSeparated1 s b
flipSeparated1Ap op (FlipSeparated1 x) (FlipSeparated1 y) =
  let (f, fs) = separated1 # x
      (a, as) = separated1 # y
  in FlipSeparated1 ((f a, zipWith (\(s, a') (t, f') -> (s `op` t, f' a')) (separated # as) (separated # fs) ^. separated) ^. separated1) 
