{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Separated.SeparatedCons(
  SeparatedCons(..)
) where

-- | Prepend a value to a separated-like structure.
class (f ~ SeparatedConsF g, g ~ SeparatedConsG f) => SeparatedCons f g where
  type SeparatedConsF g :: * -> * -> *
  type SeparatedConsG f :: * -> * -> *
  (+:) ::
    a
    -> f s a
    -> g a s

infixr 5 +:
