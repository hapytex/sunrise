module Astronomy.OrbitalSpec where

import Astronomy.Orbital(besselDenominators)

import Test.Hspec

spec :: Spec
spec = do
  describe "besselDenominators" $ do
    testBesselDenominators 1 6 [1, -2, 12, -144, 2880, -86400]
    testBesselDenominators 2 6 [1, -3, 24, -360, 8640, -302400]
    testBesselDenominators 3 6 [1, -4, 40, -720, 20160, -806400]
    testBesselDenominators 4 6 [1, -5, 60, -1260, 40320, -1814400]
    testBesselDenominators 5 6 [1, -6, 84, -2016, 72576, -3628800]
    testBesselDenominators 7 6 [1, -8, 144, -4320, 190080, -11404800]

testBesselDenominators :: Int -> Int -> [Integer] -> SpecWith (Arg Expectation)
testBesselDenominators n k exp = it ("test " ++ show n) (take k (besselDenominators n) `shouldBe` exp)

{- m   m!    n+1*..*n+m      prod
0   0!=1    2 .. 1   =  1      1
1   1!=1    2 .. 2   =  2      2
2   2!=2    2 .. 3   =  6     12
3   3!=6    2 .. 4   = 24    144
4   4!=24   2 .. 5   =120   2880
5   5!=120  2 .. 6   =720  86400

   m   m!    n+1*..*n+m       prod
0   0!=1    3 .. 2   =   1       1
1   1!=1    3 .. 3   =   3       3
2   2!=2    3 .. 4   =  12      24
3   3!=6    3 .. 5   =  60     360
4   4!=24   3 .. 6   = 360    8640
5
   m   m!    n+1*..*n+m       prod
0   0!=1    4 .. 3   =   1       1
1   1!=1    4 .. 4   =   4       4
2   2!=2    4 .. 5   =  20      40
3   3!=6    4 .. 6   = 120     720
4   4!=24   4 .. 7   = 840   20160
5   5!=120  4 .. 8   =6720  806400

   m   m!    n+1*..*n+m         prod
0   0!=1    5 .. 4   =    1        1
1   1!=1    5 .. 5   =    5        5
2   2!=2    5 .. 6   =   30       60
3   3!=6    5 .. 7   =  210     1260
4   4!=24   5 .. 8   = 1680    40320
5   5!=120  5 .. 9   =15120  1814400

   m   m!    n+1*..*n+m         prod
0   0!=1    6 ..  5   =    1         1
1   1!=1    6 ..  6   =    6         6
2   2!=2    6 ..  7   =   42        84
3   3!=6    6 ..  8   =  336      2016
4   4!=24   6 ..  9   = 3024     72576
5   5!=120  6 .. 10   =30240   3628800

m   m!    n+1*..*n+m         prod
0   0!=1    8 ..  7   =    1         1
1   1!=1    8 ..  8   =    8         8
2   2!=2    8 ..  9   =   72       144
3   3!=6    8 .. 10   =  720      4320
4   4!=24   8 .. 11   = 7920    190080
5   5!=120  8 .. 12   =95040  11404800
-}
