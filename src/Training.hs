module Training where

import Layer
import Functor

-- Loss function (MSE)
mse :: [Double] -> [Double] -> Double
mse predicted actual = 
  sum [((p - a) ** 2) | (p, a) <- zip predicted actual] / fromIntegral (length predicted)

-- Gradient of MSE
mseLossGrad :: [Double] -> [Double] -> [Double]
mseLossGrad predicted actual =
  [2 * (p - a) / fromIntegral (length predicted) | (p, a) <- zip predicted actual]

-- Single training step
trainStep :: Double -> Layer [Double] [Double] -> ([Double], [Double]) -> Layer [Double] [Double]
trainStep learningRate network (input, target) =
  let predicted = forward network input
      lossGrad = mseLossGrad predicted target
      (paramGrad, _) = backward network input lossGrad
  in updateParameters learningRate paramGrad network

-- Train for multiple epochs
train :: Int -> Double -> Layer [Double] [Double] -> [([Double], [Double])] -> Layer [Double] [Double]
train epochs learningRate network dataset =
  iterate (trainEpoch learningRate dataset) network !! epochs
  where
    trainEpoch lr ds net = foldl (trainStep lr) net ds