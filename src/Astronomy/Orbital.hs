module Astronomy.Orbital where

import Astronomy.Planet(Planet(Planet, orbitalEccentricity))
import Astronomy.Utils(Radians)

factorials :: (Enum a, Num a) => [a]
factorials = factorialsOffset 0

factorialsOffset :: (Enum a, Num a) => a -> [a]
factorialsOffset n = scanl (*) 1 [n+1 ..]

swapFactorials :: (Enum a, Num a) => [a]
swapFactorials = scanl (*) 1 [-1, -2 ..]

power2s :: Num a => a -> [a]
power2s x = scanl (*) 1 (repeat x2)
    where x2 = x*x

besselDenominators :: (Integral i, Integral j) => i -> [j]
besselDenominators n = (zipWith (*) swapFactorials (factorialsOffset (fromIntegral n)))

besselFunction' :: (Integral i, Integral j, Fractional a) => Int -> i -> j -> a -> a -> a
besselFunction' k n nfac x2n xhalf = x2n * sum (zipWith (/) (take k (power2s xhalf)) dens)
    where dens = map (fromIntegral . (nfac *)) (besselDenominators n)

-- betamFactors :: 

equationOfTheCenter' :: Floating a => a -> Radians a -> Radians a
equationOfTheCenter' e solarMean = solarMean + (2*e - 0.25*e3 + 5/96*e5 + 107/4608*e7) * sinm + (1.25 * e2 - 11/24*e4 + 17/192 * e6) * sinm2 + (13/12 * e3 - 43/64 * e5 + 95/512 * e7) * sinm3
    where e2 = e * e
          e3 = e2 * e
          e4 = e2 * e2
          e5 = e4 * e
          e6 = e4 * e2
          e7 = e6 * e
          sinm = sin solarMean
          sinm2 = sin (2*solarMean)
          sinm3 = sin (3*solarMean)

equationOfTheCenter :: Floating a => Planet a -> a -> Radians a
equationOfTheCenter Planet { orbitalEccentricity=e } = equationOfTheCenter' e

