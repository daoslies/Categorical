-- XOR neural network example (migrated from Example.hs)
module XOR where

import Layer
import Training
import System.Random
import System.Directory (createDirectoryIfMissing)
import Category ((>>>))

xorData :: [([Double], [Double])]
xorData = 
  [ ([0, 0], [0])
  , ([0, 1], [1])
  , ([1, 0], [1])
  , ([1, 1], [0])
  ]

randomWeights :: Int -> IO [Double]
randomWeights n = do
  gen <- newStdGen
  return $ take n $ randomRs (-0.5, 0.5) gen

xorNetwork :: [Double] -> Layer [Double] [Double]
xorNetwork weights =
  let (w1, rest) = splitAt 12 weights
      (w2, _) = splitAt 5 rest
  in dense 2 4 w1 >>> relu >>> dense 4 1 w2 >>> sigmoid

epoch = 10000
lr = 0.5

trainXOR :: IO ()
trainXOR = do
  weights <- randomWeights 17
  let network = xorNetwork weights
      (trained, losses) = trainWithHistory epoch lr network xorData
  createDirectoryIfMissing True "results"
  writeFile "results/XOR_losses.txt" (unlines $ map show losses)
  putStrLn ("Testing XOR network.. epochs: " ++ show epoch ++ " LR: " ++ show lr)
  putStrLn ("Loss history saved to results/XOR_losses.txt")
  putStrLn ("Final loss: " ++ show (last losses))
  mapM_ (testExample trained) xorData
  where
    testExample net (input, expected) = do
      let predicted = forward net (parameters net) input
      putStrLn $ show input ++ " -> " ++ show predicted ++ " (expected: " ++ show expected ++ ")"
