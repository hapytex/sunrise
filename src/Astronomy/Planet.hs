{-# LANGUAGE Safe #-}

module Astronomy.Planet where

import Astronomy.Utils(Radians, deg2rad)

import Data.Default(Default(def))

data Planet a = Planet {
    angularVelocity :: Radians a  -- angle/day
  , maximumAxialTilt :: Radians a  -- angle
  , argumentOfPeriapsis :: Radians a  -- angle
  , orbitalEccentricity :: a  -- factor
  }

-- https://nssdc.gsfc.nasa.gov/planetary/factsheet/mercuryfact.html
mercury :: Floating a => Planet a
mercury = Planet { angularVelocity= 0.004451859, maximumAxialTilt= deg2rad 0.034, argumentOfPeriapsis=deg2rad 77.45645, orbitalEccentricity=0.20563069 }

-- https://nssdc.gsfc.nasa.gov/planetary/factsheet/venusfact.html
venus :: Floating a => Planet a
venus = Planet { angularVelocity= -0.00107434, maximumAxialTilt= deg2rad 177.36, argumentOfPeriapsis=deg2rad 131.53298, orbitalEccentricity=0.00677323 }

-- https://nssdc.gsfc.nasa.gov/planetary/factsheet/earthfact.html
earth :: Floating a => Planet a
earth = Planet { angularVelocity= pi / 12.0, maximumAxialTilt=(deg2rad (23 + 43661 / 60000)), argumentOfPeriapsis=(deg2rad 102.94719), orbitalEccentricity=0.01671022 }

-- https://nssdc.gsfc.nasa.gov/planetary/factsheet/marsfact.html
mars :: Floating a => Planet a
mars = Planet { angularVelocity= 0.25447914, maximumAxialTilt=deg2rad 25.19, argumentOfPeriapsis=deg2rad 336.04084, orbitalEccentricity=0.09341233 }

-- https://nssdc.gsfc.nasa.gov/planetary/factsheet/jupiterfact.html
jupiter :: Floating a => Planet a
jupiter = Planet { angularVelocity= 0.636682985, maximumAxialTilt=deg2rad 3.13, argumentOfPeriapsis=deg2rad 14.75385, orbitalEccentricity=0.04839266 }

-- https://nssdc.gsfc.nasa.gov/planetary/factsheet/saturnfact.html
saturn :: Floating a => Planet a
saturn = Planet { angularVelocity= 0.612874628, maximumAxialTilt= deg2rad 26.73, argumentOfPeriapsis=deg2rad 92.43194, orbitalEccentricity=0.05415060 }

-- https://nssdc.gsfc.nasa.gov/planetary/factsheet/uranusfact.html
uranus :: Floating a => Planet a
uranus = Planet { angularVelocity= -0.363460515, maximumAxialTilt=deg2rad 97.77, argumentOfPeriapsis=deg2rad 170.96424, orbitalEccentricity=0.04716771 }

-- https://nssdc.gsfc.nasa.gov/planetary/factsheet/neptunefact.html
neptune :: Floating a => Planet a
neptune = Planet { angularVelocity= 0.388952837, maximumAxialTilt= deg2rad 28.32, argumentOfPeriapsis=deg2rad 44.97135, orbitalEccentricity=0.00858587 }

instance Floating a => Default (Planet a) where
    def = earth
