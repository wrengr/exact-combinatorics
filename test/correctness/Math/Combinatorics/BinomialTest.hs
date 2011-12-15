{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
----------------------------------------------------------------
--                                                    2011.12.14
-- |
-- Module      :  Math.Combinatorics.BinomialTest
-- Copyright   :  Copyright (c) 2011 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  Haskell98
--
----------------------------------------------------------------
module Math.Combinatorics.BinomialTest where
import Math.Combinatorics.Factorial (factorial_naive)
import Math.Combinatorics.Binomial  (binomial)

import qualified Test.QuickCheck as QC
import qualified Test.SmallCheck as SC

----------------------------------------------------------------

-- BUG: both QC and SC claim (13`choose`1) fails, even though both functions produce 13... The 'prop_binomial' returns False, though if we call its body directly, then it returns True. wtf? it's something reliable about (13`choose`1) though...

-- BUG: QC claims (141`choose`0), (91`choose`0), (18`choose`1), (27`choose`1),... fails. Again, calling the body of 'prop_binomial' works fine, bug calling 'prop_binomial' directly gives that strande division-by-zero bug. wtf?--- ah, maybe (n-k)<0 causes a zero denominator in 'quot'? But the direct call works fine...

main :: IO ()
main = do
    QC.quickCheck    prop_binomial
    SC.smallCheck 20 prop_binomial

----------------------------------------------------------------

-- | The naive implementation: @n! / (k! * (n-k)!)@.
binomial_naive :: (Integral a) => a -> a -> a
{-# SPECIALIZE binomial_naive ::
    Integer -> Integer -> Integer,
    Int -> Int -> Int
    #-}
binomial_naive n k
    | k <  0    = 0
    | k >  n    = 0
    | otherwise =
        factorial_naive n `quot` (factorial_naive k * factorial_naive (n-k))


-- TODO: improve the seach space with (==>), SmallCheck.Nat, and QuickCheck.NonNegative
prop_binomial :: Int -> Int -> Bool
prop_binomial n k = binomial_naive n k == binomial n k

----------------------------------------------------------------
----------------------------------------------------------- fin.
