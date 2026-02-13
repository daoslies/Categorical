{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Orbital where
import qualified GHC.TypeLits as TL
import GHC.TypeLits (Nat)

import Linear.V3
import Linear.V4

-- Orbital angular momentum quantum numbers (simplified)


data L = S | P | D | Hybrid Nat

type family OrbitalDim (l :: L) :: Nat where
    OrbitalDim 'S = 1
    OrbitalDim 'P = 3
    OrbitalDim ('Hybrid n) = n TL.+ 1

-- Value-level angle via explicit pattern matching
orbitalAngle :: Orbital l -> Double
orbitalAngle (SP _) = 180.0
orbitalAngle (SP2 _) = 120.0
orbitalAngle (SP3 _) = 109.4712206

data Orbital (l :: L) where
    PureS :: Orbital 'S
    PureP :: V3 Double -> Orbital 'P  -- Uses linear package
    SP    :: Double -> Orbital ('Hybrid 1)  -- Mixing coefficient
    SP2   :: V3 Double -> Orbital ('Hybrid 2)  -- Normalized vector in simplex
    SP3   :: V4 Double -> Orbital ('Hybrid 3)  -- Normalized vector in simplex

-- | Smart constructor for standard sp hybrid along z-axis (standard orientation)
standardSP :: Orbital ('Hybrid 1)
standardSP = SP (1 / sqrt 2)  -- c_s = 1/√2, c_p = 1/√2

-- | Smart constructor for standard sp2 hybrid in xy-plane (trigonal)
--   n = 0,1,2 for the three equivalent hybrids
standardSP2 :: Int -> Orbital ('Hybrid 2)
standardSP2 n = 
    let c_s = 1 / sqrt 3
        angle = fromIntegral n * 2 * pi / 3  -- 0°, 120°, 240°
        c_px = sqrt (2/3) * cos angle
        c_py = sqrt (2/3) * sin angle
    in SP2 (V3 c_s c_px c_py)

-- | Smart constructor for standard sp3 hybrid orbital pointing in a given direction.
--   The direction vector must be normalized.
standardSP3 :: V3 Double -> Orbital ('Hybrid 3)
standardSP3 (V3 x y z) =
    let c_s = 0.5  -- always 1/4 s character (1/2 squared)
        c_p = sqrt 3 / 2  -- 3/4 p character
    in SP3 (V4 c_s (c_p*x) (c_p*y) (c_p*z))

deriving instance Show (Orbital l)

-- Existential for runtime type recovery
data SomeOrbital where
    SomeOrbital :: Orbital l -> SomeOrbital
    
deriving instance Show SomeOrbital