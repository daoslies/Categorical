module Example where

import Layer
import Training
import Category

-- XOR dataset
xorData :: [([Double], [Double])]
xorData = 
  [ ([0, 0], [0])
  , ([0, 1], [1])
  , ([1, 0], [1])
  , ([1, 1], [0])
  ]

-- Build a simple network: 2 -> 4 -> 1
-- Using categorical composition
xorNetwork :: [Double] -> Layer [Double] [Double]
xorNetwork weights =
  let (w1, rest) = splitAt 8 weights   -- 2*4 weights for first layer
      (w2, _) = splitAt 4 rest          -- 4*1 weights for second layer
  in dense 2 4 w1 >>> relu >>> dense 4 1 w2 >>> sigmoid

-- Initialize with random weights
initialWeights :: [Double]
initialWeights = replicate 12 0.5  -- Simple initialization

-- Train the network

epoch = 10000
lr = 0.001

trainXOR :: IO ()
trainXOR = do
  let network = xorNetwork initialWeights
      trained = train epoch lr network xorData
  
  putStrLn ("Testing XOR network.. epochs: " ++ show epoch ++ " LR: " ++ show lr)
  mapM_ (testExample trained) xorData
  where
    testExample net (input, expected) = do
      let predicted = forward net input
      putStrLn $ show input ++ " -> " ++ show predicted ++ " (expected: " ++ show expected ++ ")"