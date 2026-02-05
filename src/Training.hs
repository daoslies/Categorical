module Training where

import Layer
import Functor
import Debug.Trace

-- Loss function (MSE)
mse :: [Double] -> [Double] -> Double
mse predicted actual = 
  sum [((p - a) ** 2) | (p, a) <- zip predicted actual] / fromIntegral (length predicted)

-- Gradient of MSE
mseLossGrad :: [Double] -> [Double] -> [Double]
mseLossGrad predicted actual =
  [2 * (p - a) / fromIntegral (length predicted) | (p, a) <- zip predicted actual]

-- Single training step
-- Passes parameters explicitly to forward and backward
trainStep :: Double -> Layer [Double] [Double] -> ([Double], [Double]) -> Layer [Double] [Double]
trainStep learningRate network (input, target) =
  let params = parameters network
      predicted = forward network params input  -- Pass params!
      lossGrad = mseLossGrad predicted target
      (paramGrad, _) = backward network params input lossGrad  -- Pass params!
      updated = updateParameters learningRate paramGrad network
  in trace ("Params before: " ++ show (take 3 $ parameters network) ++    -- uncomment to print params during training
            " after: " ++ show (take 3 $ parameters updated)) updated

-- Train for multiple epochs
train :: Int -> Double -> Layer [Double] [Double] -> [([Double], [Double])] -> Layer [Double] [Double]
train epochs learningRate network dataset =
  iterate (trainEpoch learningRate dataset) network !! epochs
  where
    trainEpoch lr ds net = foldl (trainStep lr) net ds