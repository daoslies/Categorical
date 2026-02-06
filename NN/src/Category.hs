module Category where

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- A category where morphisms are parameterized by input/output types
class Category k where
  -- Identity morphism
  ident :: k a a
  
  -- Composition (note: we use >>> for left-to-right composition)
  (>>>) :: k a b -> k b c -> k a c

-- Category of Haskell functions (for reference)
instance Category (->) where
  ident = id
  (>>>) = flip (.)

-- Monoidal category (for parallel composition)
class Category k => Monoidal k where
  -- Tensor product of morphisms (run two layers in parallel)
  (***) :: k a b -> k c d -> k (a, c) (b, d)
