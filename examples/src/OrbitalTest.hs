module OrbitalTest where

import Linear.V3 (V3(..))
import Linear.V4 (V4(..))

import Orbital
import Geometry

testOrbitals :: IO ()
testOrbitals = do
  putStrLn "Testing Orbital Types:"
  
  let sp3 = SP3 (V4 0.5 0.3 0.15 0.05)
  putStrLn $ "SP3 orbital: " ++ show sp3
  putStrLn $ "As vector: " ++ show (toVector sp3)
  putStrLn $ "Bond angle: " ++ show (bondAngle sp3) ++ "°"
  
  let sp2 = SP2 (V3 0.33 0.33 0.34)
  putStrLn $ "\nSP2 orbital: " ++ show sp2
  putStrLn $ "As vector: " ++ show (toVector sp2)
  putStrLn $ "Bond angle: " ++ show (bondAngle sp2) ++ "°"
  
  putStrLn "\nType system prevents invalid orbitals at compile time!"


  putStrLn "=== Testing Orbital Smart Constructors ==="
  
  -- Test SP
  let sp = standardSP
  putStrLn $ "Standard SP: " ++ show sp
  putStrLn $ "Vector: " ++ show (toVector sp)
  putStrLn $ "Angle: " ++ show (orbitalAngle sp) ++ "°"
  
  -- Test SP2
  putStrLn "\nSP2 hybrids (should be 120° apart):"
  mapM_ (\n -> do
    let sp2 = standardSP2 n
    putStrLn $ "  SP2(" ++ show n ++ "): " ++ show (toVector sp2)
    ) [0..2]
  
  -- Test SP3
  putStrLn "\nSP3 pointing along z-axis:"
  let sp3 = standardSP3 (V3 0 0 1)
  putStrLn $ "SP3: " ++ show (toVector sp3)
  putStrLn $ "Angle: " ++ show (orbitalAngle sp3) ++ "°"
  
  -- Test normalization
  putStrLn "\n=== Testing Normalization ==="
  let vec = toVector sp3
      norm = sum [x*x | x <- vec]
  putStrLn $ "SP3 norm² = " ++ show norm ++ " (should be 1.0)"