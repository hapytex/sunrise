{-# LANGUAGE NumericUnderscores, Safe #-}

module Astronomy.Utils where

import Data.Time.Calendar(Day(ModifiedJulianDay))
import Data.Time.Clock(DiffTime, UTCTime, picosecondsToDiffTime)

type Degrees a = a
type Radians a = a

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

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

toUTCTime :: (Ord a, Floating a) => a -> UTCTime
toUTCTime = undefined

floatingToTime :: (Ord a, Floating a) => a -> DiffTime
floatingToTime = picosecondsToDiffTime . _scale 0 86_400_000_000_000_000

_scale :: (Floating a, Ord a) => Integer -> Integer -> a -> Integer
_scale mi mx v
    | mi >= mx = mi
    | v < 0.5 = _scale mi md (2*v)
    | otherwise = _scale md mx (2.0*(v - 0.5))
    where md = div (mi + mx) 2

-- fromNumberOfDays :: Floating a => a -> UTCTime
-- fromNumberOfDays n = 10000 * n - 515444992
