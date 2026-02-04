{-# LANGUAGE RankNTypes #-}

module Functor where

import Layer

-- A functor from the category of Layers to itself
-- This represents transformations of networks
newtype NetworkFunctor = NetworkFunctor
  { applyFunctor :: forall a b. Layer a b -> Layer a b
  }

-- Natural transformation: a way to transform between functors
-- For example, parameter initialization or normalization
type NaturalTransformation f g = forall a b. f a b -> g a b

-- Batch normalization as a natural transformation
-- (Simplified version - just shifts outputs)
batchNorm :: Double -> Double -> NaturalTransformation Layer Layer
batchNorm mean std layer = Layer
  { parameters = parameters layer ++ [mean, std]
  , forward = \x ->
      let y = forward layer x
          normalized = map (\v -> (v - mean) / std) y
      in normalized
  , backward = \x dy ->
      backward layer x dy ++ [0, 0]  -- Simplified gradient
  }

-- Parameter update as a functor
-- Maps a network to an updated network
updateParameters :: Double -> [Double] -> Layer a b -> Layer a b
updateParameters learningRate gradients layer =
  let oldParams = parameters layer
      newParams = zipWith (\p g -> p - learningRate * g) oldParams gradients
  in layer { parameters = newParams }