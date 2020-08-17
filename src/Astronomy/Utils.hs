{-# LANGUAGE Safe #-}

module Astronomy.Utils where

import Data.Time.Calendar(Day(ModifiedJulianDay))
import Data.Time.Clock(UTCTime(UTCTime), diffTimeToPicoseconds)

type Degrees a = a
type Radians a = a

pi2 :: Floating a => Radians a
pi2 = 2*pi

deg2rad :: Floating a => Degrees a -> Radians a
deg2rad = (pi / 180.0 *)

rad2turns :: Floating a => Radians a -> a
rad2turns = (0.5 / pi *)

cossin :: Floating a => a -> a
cossin x = sqrt (1 - x*x)

floatMod :: (Floating a, Ord a) => a -> a -> a
floatMod f m
    | f < 0 = floatMod (f + m) m
    | f > m = floatMod (f - m) m
    | otherwise = f

toNumberOfDays :: Floating a => Day -> a
toNumberOfDays (ModifiedJulianDay mjd) = 0.0001 * fromIntegral (10000 * mjd - 515444992) -- + fromIntegral (diffTimeToPicoseconds s) / 86401000000000000.0

-- fromNumberOfDays :: Floating a => a -> UTCTime
-- fromNumberOfDays n = 10000 * n - 515444992
