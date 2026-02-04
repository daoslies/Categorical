{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Layer where

import Category

-- A Layer is a morphism that also carries:
-- 1. Parameters (weights)
-- 2. Gradient computation 
-- 3. Forward pass
data Layer a b = Layer

  { parameters :: [Double]           -- Learnable parameters
  , forward :: a -> b                -- Forward computation
  , backward :: a -> b -> ([Double], a)   -- Gradient w.r.t. parameters
  }

-- Layers form a category
instance Category Layer where

  ident = Layer
    { parameters = []
    , forward = id
    , backward = \_ grad -> ([], grad)
    }
  
  -- Composition of layers (l1 >>> l2)
  l1 >>> l2 = Layer
    { parameters = parameters l1 ++ parameters l2
    , forward = forward l2 . forward l1
    , backward = \x dy ->
        let y1 = forward l1 x
            (grads2, dx2) = backward l2 y1 dy
            (grads1, dx1) = backward l1 x dx2
        in (grads1 ++ grads2, dx1)
    }

-- Dense (fully connected) layer
dense :: Int -> Int -> [Double] -> Layer [Double] [Double]
dense inSize outSize weights = Layer
  { parameters = weights
  , forward = \input ->
      let matrix = chunksOf inSize weights
      in map (\row -> sum $ zipWith (*) row input) (take outSize matrix)
  , backward = \input grad ->
      let paramGrads = concat [[g * x | x <- input] | g <- grad]
          matrix = chunksOf inSize weights
          inputGrad = [sum [g * w | (g, w) <- zip grad (map (!!i) matrix)] | i <- [0..inSize-1]]
      in (paramGrads, inputGrad)
  }
  where
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Activations

activation :: (Double -> Double) -> Layer [Double] [Double]
activation f = Layer
  { parameters = []
  , forward = map f
  , backward = \_ grad -> ([], grad)
  }

relu :: Layer [Double] [Double]
relu = activation (\x -> max 0 x)

sigmoid :: Layer [Double] [Double]
sigmoid = activation (\x -> 1 / (1 + exp (-x)))