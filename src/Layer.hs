{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Layer where

import Category

-- Helper to split chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- A Layer is a morphism that also carries:
-- 1. Parameters (weights)
-- 2. Gradient computation 
-- 3. Forward pass
-- Now includes explicit parameter passing for forward/backward
-- and a rebuild function for training

data Layer a b = Layer
  { parameters :: [Double]           -- Learnable parameters
  , forward :: [Double] -> a -> b    -- Forward computation (params explicit)
  , backward :: [Double] -> a -> b -> ([Double], a)   -- Gradient w.r.t. parameters (params explicit)
  }

-- Layers form a category
instance Category Layer where
  ident = Layer
    { parameters = []
    , forward = \_ x -> x
    , backward = \_ _ grad -> ([], grad)
    }
  
  -- Composition of layers
  l1 >>> l2 = Layer
    { parameters = parameters l1 ++ parameters l2
    , forward = \params x ->
        let (p1, p2) = splitAt (length $ parameters l1) params
            y1 = forward l1 p1 x
        in forward l2 p2 y1
    , backward = \params x dy ->
        let (p1, p2) = splitAt (length $ parameters l1) params
            y1 = forward l1 p1 x
            (grads2, dx2) = backward l2 p2 y1 dy
            (grads1, dx1) = backward l1 p1 x dx2
        in (grads1 ++ grads2, dx1)
    }

-- Dense (fully connected) layer
-- forward/backward take params as argument
-- paramGrads: gradient w.r.t. weights
-- inputGrad: gradient w.r.t. input
-- Added Biases
-- Weights: inSize * outSize, Biases: outSize
dense :: Int -> Int -> [Double] -> Layer [Double] [Double]
dense inSize outSize initialParams = 
  let numWeights = inSize * outSize
      numBiases = outSize
  in Layer
    { parameters = initialParams
    , forward = \params input ->
        let (weights, biases) = splitAt numWeights params
            matrix = chunksOf inSize weights
            weightedSum = map (\row -> sum $ zipWith (*) row input) (take outSize matrix)
        in zipWith (+) weightedSum biases  -- Add biases!
    , backward = \params input grad ->
        let (weights, _biases) = splitAt numWeights params
            -- Gradient w.r.t. weights
            weightGrads = concat [[g * x | x <- input] | g <- grad]
            -- Gradient w.r.t. biases (just the incoming gradient)
            biasGrads = grad
            -- Gradient w.r.t. inputs
            matrix = chunksOf inSize weights
            inputGrad = [sum [g * w | (g, w) <- zip grad (map (!!i) matrix)] 
                        | i <- [0..inSize-1]]
        in (weightGrads ++ biasGrads, inputGrad)
    }

-- Activations
-- No parameters, just apply function elementwise
{-activation :: (Double -> Double) -> Layer [Double] [Double]
activation f = Layer
  { parameters = []
  , forward = \_ -> map f
  , backward = \_ _ grad -> ([], grad)
  }
-}
-- ReLU with proper gradient
relu :: Layer [Double] [Double]
relu = Layer
  { parameters = []
  , forward = \_ input -> map (\x -> max 0 x) input
  , backward = \_ input grad -> 
      let reluGrad = zipWith (\x g -> if x > 0 then g else 0) input grad
      in ([], reluGrad)
  }

-- Sigmoid with proper gradient
sigmoid :: Layer [Double] [Double]
sigmoid = Layer
  { parameters = []
  , forward = \_ input -> map (\x -> 1 / (1 + exp (-x))) input
  , backward = \_ input grad ->
      let sigmoidOutputs = map (\x -> 1 / (1 + exp (-x))) input
          sigmoidGrad = zipWith3 (\s g _ -> g * s * (1 - s)) sigmoidOutputs grad input
      in ([], sigmoidGrad)
  }