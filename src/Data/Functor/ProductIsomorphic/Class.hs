{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Data.Functor.ProductIsomorphic.Class
-- Copyright   : 2017 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines functor interfaces which morphed functions
-- are restricted to products.
module Data.Functor.ProductIsomorphic.Class
       ( ProductIsoFunctor (..)
       , ProductIsoApplicative (..)
       , ProductIsoAlternative (..)
       ) where

import Data.Functor.ProductIsomorphic.Unsafe (ProductConstructor)

-- | Restricted functor on products.
class ProductIsoFunctor f where
  (|$|) :: ProductConstructor (a -> b) => (a -> b) -> f a -> f b

-- | Restricted applicative functor on products.
class ProductIsoFunctor f => ProductIsoApplicative f where
  pureP :: ProductConstructor a => a -> f a
  (|*|) :: f (a -> b) -> f a -> f b

-- | Restricted alternative on products.
class ProductIsoApplicative f => ProductIsoAlternative f where
  emptyP :: f a
  (|||)  :: f a -> f a -> f a

infixl 4 |$|, |*|
infixl 3 |||
