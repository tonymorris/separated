{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Separated.FlipSeparated(
) where

import Control.Applicative(Applicative(pure, (<*>)))
import Control.Category
import Control.Lens hiding ((<.>))
import Data.Bifunctor
import Data.Eq(Eq)
import Data.Functor
import Data.Functor.Apply
import Data.List(zipWith)
import Data.Ord(Ord)
import Data.Semigroup
import Data.Separated.FlipSeparatedCons
import Data.Separated.Separated
import Data.Separated.SeparatedCons
import Prelude(Show(show))

newtype FlipSeparated a s =
  FlipSeparated (Separated s a)
  deriving (Eq, Ord)

instance Bifunctor FlipSeparated where
  bimap f g (FlipSeparated x) =
    FlipSeparated (bimap g f x)

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
    (s +: p ^. flipSeparated1) ^. from flipSeparated

-- | The isomorphism to a @Separator@.
--
-- >>> flipSeparated # empty
-- []
--
-- >>> flipSeparated # ('x' +: 6 +: empty)
-- ['x',6]
--
-- >>> [] ^. separated . from flipSeparated
-- []
--
-- >>> [(6, [])] ^. separated . from flipSeparated
-- [6,[]]
flipSeparated ::
  Iso (FlipSeparated a s) (FlipSeparated b t) (Separated s a) (Separated t b)
flipSeparated =
  iso (\(FlipSeparated x) -> x) FlipSeparated

----

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
-- >>> flipSeparated1 # (single 6)
-- [6]
--
-- >>> flipSeparated1 # (5 +: 'x' +: single 6)
-- [5,'x',6]
--
-- >>> (6 +: empty) ^. from flipSeparated1
-- [6]
--
-- >>> (5 +: 'x' +: 6 +: empty) ^. from flipSeparated1
-- [5,'x',6]
flipSeparated1 ::
  Iso (FlipSeparated1 s a) (FlipSeparated1 t b) (Separated1 a s) (Separated1 b t)
flipSeparated1 =
  iso (\(FlipSeparated1 x) -> x) FlipSeparated1

instance FlipSeparatedCons FlipSeparated FlipSeparated1 where
  type FlipSeparatedConsF FlipSeparated1 = FlipSeparated
  type FlipSeparatedConsG FlipSeparated = FlipSeparated1
  a +. p =
    (a +: p ^. flipSeparated) ^. from flipSeparated1

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
