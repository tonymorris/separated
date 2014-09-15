{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Separated.FlipSeparated(
) where

import Control.Applicative
import Control.Category
import Control.Lens hiding ((<.>))
import Data.Bifunctor
import Data.Eq(Eq)
import Data.Functor
import Data.Functor.Apply
import Data.Ord(Ord)
import Data.Semigroup
import Data.Separated.Separated
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
{-}
instance SeparatedCons Separated1 Separated where
  type SeparatedConsF Separated = Separated1
  type SeparatedConsG Separated1 = Separated
  s +: Separated1 a (Separated x) =
    Separated ((s, a) : x)
-}


newtype FlipSeparated1 s a =
  FlipSeparated1 (Separated1 a s)

instance Bifunctor FlipSeparated1 where
  bimap f g (FlipSeparated1 x) =
    FlipSeparated1 (bimap g f x)

instance Functor (FlipSeparated1 a) where
  fmap =
    bimap id
{-}
instance Semigroup a => Apply (FlipSeparated1 a) where
  FlipSeparated1 x <.> FlipSeparated1 y =
    FlipSeparated1 (separatedSwap # (x ^. separatedSwap <.> y ^. separatedSwap))

instance Monoid s => Applicative (FlipSeparated1 s) where    
  FlipSeparated1 x <*> FlipSeparated1 y =
    FlipSeparated1 (separatedSwap # (x ^. separatedSwap <*> y ^. separatedSwap))
  pure =
    FlipSeparated1 . (#) separatedSwap . pure

instance (Show s, Show a) => Show (FlipSeparated1 s a) where
  show (FlipSeparated1 x) =
    show x

-}