{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Geometry where

import Orbital
import GHC.TypeLits (Nat, KnownNat, natVal)
import Data.Proxy
import Linear.V3 (V3(..))
import Linear.V4 (V4(..))

-- Convert orbital to coefficient vector
toVector :: Orbital l -> [Double]
toVector PureS = [1.0]
toVector (PureP (V3 x y z)) = [x, y, z]
toVector (SP c_s) = [c_s, sqrt (1 - c_s*c_s)]
toVector (SP2 (V3 c_s c_p1 c_p2)) = [c_s, c_p1, c_p2]
toVector (SP3 (V4 c_s c_px c_py c_pz)) = [c_s, c_px, c_py, c_pz]

-- Attempt to reconstruct orbital from vector
-- Returns Nothing if vector doesn't satisfy normalization
fromVector :: [Double] -> Maybe SomeOrbital
fromVector [x]
  | abs (x - 1.0) < 1e-6 = Just (SomeOrbital PureS)
  | otherwise = Nothing
fromVector [x, y, z]
  | abs (x*x + y*y + z*z - 1.0) < 1e-6 = Just (SomeOrbital (PureP (V3 x y z)))
  | otherwise = Nothing
fromVector [c_s, c_p]
  | abs (c_s*c_s + c_p*c_p - 1.0) < 1e-6 = Just (SomeOrbital (SP c_s))
  | otherwise = Nothing
fromVector [c_s, c_p1, c_p2]
  | abs (c_s*c_s + c_p1*c_p1 + c_p2*c_p2 - 1.0) < 1e-6 = Just (SomeOrbital (SP2 (V3 c_s c_p1 c_p2)))
  | otherwise = Nothing
fromVector [c_s, c_px, c_py, c_pz]
  | abs (c_s*c_s + c_px*c_px + c_py*c_py + c_pz*c_pz - 1.0) < 1e-6 = Just (SomeOrbital (SP3 (V4 c_s c_px c_py c_pz)))
  | otherwise = Nothing
fromVector _ = Nothing

-- Compute bond angle from orbital geometry
bondAngle :: Orbital ('Hybrid n) -> Double
bondAngle (SP _) = 180.0
bondAngle (SP2 _) = 120.0
bondAngle (SP3 _) = 109.47