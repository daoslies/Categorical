{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Layer where

import Category
import qualified Data.Vector as V

-- A Layer is a morphism that also carries:
-- 1. Parameters (weights)
-- 2. Gradient computation 
-- 3. Forward pass
data Layer a b = Layer

  { parameters :: [Double]           -- Learnable parameters
  , forward :: a -> b                -- Forward computation
  , backward :: a -> b -> [Double]   -- Gradient w.r.t. parameters
  }

-- Layers form a category
instance Category Layer where

  ident = Layer
    { parameters = []
    , forward = id
    , backward = \_ _ -> []
    }
  
  -- Composition of layers
  l2 >>> l1 = Layer
    { parameters = parameters l1 ++ parameters l2
    , forward = forward l2 . forward l1
    , backward = \x dy ->
        let y1 = forward l1 x
            grad1 = backward l1 x dy
            grad2 = backward l2 y1 dy
        in grad1 ++ grad2
    }

-- Dense (fully connected) layer
dense :: Int -> Int -> [Double] -> Layer [Double] [Double]
dense inSize outSize weights = Layer
{ parameters = weights
, forward = \input ->
    let matrix = chunksOf inSize weights
    in map (\row -> sum $ zipWith (*) row input) (take outSize matrix) -- dot product
, backward = \input grad ->
    concat [[g * x | x <- input] | g <- grad]
}
where
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)



-- Activations

relu :: Layer [Double] [Double]
relu = Layer
  { parameters = []
  , forward = map (\x -> max 0 x)
  , backward = \_ _ -> []
  }

sigmoid :: Layer [Double] [Double]
sigmoid = activation (\x -> 1 / (1 + exp (-x)))