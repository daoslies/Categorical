module Example where

import Layer
import Training
import Category

import System.Random
import System.Directory (createDirectoryIfMissing)

-- XOR dataset
xorData :: [([Double], [Double])]
xorData = 
  [ ([0, 0], [0])
  , ([0, 1], [1])
  , ([1, 0], [1])
  , ([1, 1], [0])
  ]
  

-- Initialize weights:
-- Layer 1: 2*4 weights + 4 biases = 12
-- Layer 2: 4*1 weights + 1 bias = 5
-- Total: 17 parameters

randomWeights :: Int -> IO [Double]
randomWeights n = do
  gen <- newStdGen
  return $ take n $ randomRs (-0.5, 0.5) gen

xorNetwork :: [Double] -> Layer [Double] [Double]
xorNetwork weights =
  let (w1, rest) = splitAt 12 weights   -- 2*4 + 4 biases
      (w2, _) = splitAt 5 rest           -- 4*1 + 1 bias
  in dense 2 4 w1 >>> relu >>> dense 4 1 w2 >>> sigmoid



-- hyperparams
epoch = 10000
lr = 0.5


trainXOR :: IO ()
trainXOR = do
  weights <- randomWeights 17
  
  let network = xorNetwork weights
      (trained, losses) = trainWithHistory epoch lr network xorData
  
  -- Ensure results directory exists
  createDirectoryIfMissing True "results"
  -- Save losses to file
  writeFile "results/XOR_losses.txt" (unlines $ map show losses)
  
  putStrLn ("Testing XOR network.. epochs: " ++ show epoch ++ " LR: " ++ show lr)
  putStrLn ("Loss history saved to results/losses.txt")
  putStrLn ("Final loss: " ++ show (last losses))
  
  mapM_ (testExample trained) xorData
  where
    testExample net (input, expected) = do
      let predicted = forward net (parameters net) input
      putStrLn $ show input ++ " -> " ++ show predicted ++ " (expected: " ++ show expected ++ ")"
