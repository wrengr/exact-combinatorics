{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
----------------------------------------------------------------
--                                                    2021.10.17
-- |
-- Module      :  Math.Combinatorics.BinomialTest
-- Copyright   :  Copyright (c) 2011--2021 wren gayle romano
-- License     :  BSD
-- Maintainer  :  wren@cpan.org
-- Stability   :  experimental
-- Portability :  Haskell98
--
----------------------------------------------------------------
module Math.Combinatorics.BinomialTest where
import Math.Combinatorics.Binomial (choose)

import qualified Test.QuickCheck as QC
import qualified Test.SmallCheck as SC

----------------------------------------------------------------

main :: IO ()
main = do
    SC.smallCheck 21 prop_binomial -- overflow and out-of-bounds checking
    SC.smallCheck 100
        (\n k -> cond_binomialIsNonZero n k SC.==> prop_binomial n k)
    -- QC.quickCheck prop_binomial -- Need a better generator

----------------------------------------------------------------

-- | The naive but obviously correct implementation.
factorial_naive :: (Integral a) => a -> a
{-# SPECIALIZE factorial_naive ::
    Integer -> Integer,
    Int -> Int
    #-}
factorial_naive n
    | n < 0     = 0
    | otherwise = product [1..n]

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
-- N.B., to avoid bizarro bugs where this prop returns false even though calling its body directly returns true, we must use Integer. The bizarro bugs are caused by integer overflow (12! is the largest that can fit into Int32; 20! is largest tfor Int64)
prop_binomial :: Integer -> Integer -> Bool
prop_binomial n k = binomial_naive n k == (n `choose` k)

cond_binomialIsNonZero ::  Integer -> Integer -> Bool
cond_binomialIsNonZero n k = (n > 0) && (k > 0) && (n >= k)

----------------------------------------------------------------
----------------------------------------------------------- fin.
