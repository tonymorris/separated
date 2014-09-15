{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Separated.FlipSeparatedCons(
  FlipSeparatedCons(..)
) where

-- | Prepend a value to a separated-like structure.
class (f ~ FlipSeparatedConsF g, g ~ FlipSeparatedConsG f) => FlipSeparatedCons f g where
  type FlipSeparatedConsF g :: * -> * -> *
  type FlipSeparatedConsG f :: * -> * -> *
  (+.) ::
    s
    -> f s a
    -> g a s

infixr 5 +.
