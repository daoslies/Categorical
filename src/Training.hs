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

-- Single training step (now returns loss too)
trainStep :: Double -> Layer [Double] [Double] -> ([Double], [Double]) -> (Layer [Double] [Double], Double)
trainStep learningRate network (input, target) =
  let params = parameters network
      predicted = forward network params input
      loss = mse predicted target
      lossGrad = mseLossGrad predicted target
      (paramGrad, _) = backward network params input lossGrad
      updated = updateParameters learningRate paramGrad network
  in (updated, loss)
--  in (trace ("Params before: " ++ show (take 3 $ parameters network) ++    -- uncomment to print params during training
--            " after: " ++ show (take 3 $ parameters updated)) updated, loss)

-- Train and return loss history
trainWithHistory :: Int -> Double -> Layer [Double] [Double] -> [([Double], [Double])] -> (Layer [Double] [Double], [Double])
trainWithHistory epochs learningRate network dataset =
  let results = take (epochs + 1) $ iterate (trainEpoch learningRate dataset) (network, [])
      (finalNet, lossHistory) = last results
  in (finalNet, reverse lossHistory)
  where
    trainEpoch lr ds (net, losses) = 
      let (newNet, epochLoss) = foldl (step lr) (net, 0) ds
          avgLoss = epochLoss / fromIntegral (length ds)
      in (newNet, avgLoss : losses)
    
    step lr (net, totalLoss) example =
      let (newNet, loss) = trainStep lr net example
      in (newNet, totalLoss + loss)