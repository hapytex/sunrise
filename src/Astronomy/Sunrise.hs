{-# LANGUAGE Safe #-}

module Astronomy.Sunrise where

import Astronomy.Planet(Planet(Planet, maximumAxialTilt, argumentOfPeriapsis))
import Astronomy.Utils(cossin, deg2rad, floatMod, pi2, rad2turns, toNumberOfDays)

import Data.Time.Calendar(Day(ModifiedJulianDay))
import Data.Time.Clock(UTCTime)

--sunrise :: (Floating a, Ord a) => Day -> (a, a) -> Planet a -> (a, a)
sunrise = _sunrise . toNumberOfDays

sinCorrection :: Floating a => a
sinCorrection = sin (deg2rad (-0.83))

_sunrise :: (Floating a, Ord a) => a -> (a, a) -> Planet a -> (a, a)
_sunrise n (latitude, longitude) Planet {maximumAxialTilt=mat, argumentOfPeriapsis=arpe} = _jSetRise jtransit w0
    where w0 = _hourAngle latitude sindec
          jstar = _meanSolarNoon n longitude
          lambda = _eclipticLongitude meanam eqofc arpe
          jtransit = _solarNoon jstar meanam lambda
          meanam = _solarMeanAnomaly jstar
          eqofc = _equationOfCenter meanam
          sindec = _sinDeclination lambda mat

_sinDeclination :: Floating a => a -> a -> a
_sinDeclination lambda mat = sin lambda * sin mat

_hourAngle :: Floating a => a -> a -> a
_hourAngle latitude sindec = acos ((sinCorrection - sin latitude * sindec) / (cos latitude * cossin sindec))

_jSetRise :: Floating a => a -> a -> (a, a)
_jSetRise jtransit w0 = (jtransit - rad2turns w0, jtransit + rad2turns w0)

_solarMeanAnomaly :: (Floating a, Ord a) => a -> a
_solarMeanAnomaly jstar = floatMod (deg2rad 357.5291 + deg2rad 0.98560028 * jstar) pi2

_solarNoon :: Floating a => a -> a -> a -> a
_solarNoon jstar m lambda = 2451545.0 + jstar + 0.0053 * sin m - 0.0069 * sin (2 * lambda)

_meanSolarNoon :: Floating a => a -> a -> a
_meanSolarNoon n lw = n - rad2turns lw

_eclipticLongitude :: (Floating a, Ord a) => a -> a -> a -> a
_eclipticLongitude m c arpe = floatMod (m + c + pi + arpe) pi2

meanSolarNoon :: Floating a => Day -> a -> a
meanSolarNoon utc = _meanSolarNoon (toNumberOfDays utc)

_equationOfCenter :: Floating a => a -> a
_equationOfCenter m = deg2rad 1.9148 * sin m + deg2rad 0.02 * sin (2*m) + deg2rad 0.0003 * sin (3*m)