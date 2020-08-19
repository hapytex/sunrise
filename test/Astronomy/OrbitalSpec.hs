module Astronomy.OrbitalSpec where

import Astronomy.Orbital(besselDenominators)

import Test.Hspec

spec :: Spec
spec = do
  describe "besselDenominators" $ do
    testBesselDenominators 1 5 [1]
    testBesselDenominators 2 5 [1]
    testBesselDenominators 3 5 [1]
    testBesselDenominators 4 5 [1]
    testBesselDenominators 5 5 [1]
    testBesselDenominators 7 5 [1]

testBesselDenominators :: Int -> Int -> [Integer] -> SpecWith (Arg Expectation)
testBesselDenominators n k exp = it ("test " ++ show n) (take k (besselDenominators n) `shouldBe` exp)
