{-# LANGUAGE Safe #-}

module Astronomy.Planet where

import Astronomy.Utils(deg2rad)

import Data.Default(Default(def))

data Planet a = Planet {
    angularVelocity :: a
  , maximumAxialTilt :: a
  , argumentOfPeriapsis :: a
  }

earth :: Floating a => Planet a
earth = Planet (pi / 12.0) (deg2rad (23 + 43661 / 60000)) (deg2rad 102.9372)

instance Floating a => Default (Planet a) where
    def = earth
