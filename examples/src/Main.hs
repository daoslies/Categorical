-- Main entry point for running examples
module Main where

import qualified XOR
import qualified OrbitalTest

main :: IO ()
main = do
  putStrLn "=== XOR Example ==="
  XOR.trainXOR
  
  putStrLn "\n=== Orbital Type System ==="
  OrbitalTest.testOrbitals