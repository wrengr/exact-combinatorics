{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
----------------------------------------------------------------
--                                                    2011.12.04
-- |
-- Module      :  Math.Combinatorics.Factorial
-- Copyright   :  Copyright (c) 2011 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  Haskell98
--
-- The factorial numbers <http://oeis.org/A000142>. For negative inputs, all functions return 0 (rather than throwing an exception or using 'Maybe').
--
-- Limits:
--
-- * 12! is the largest value that can fit in 'Data.Int.Int32'
--
-- * 20! is the largest value that can fit in 'Data.Int.Int64'
----------------------------------------------------------------
module Math.Combinatorics.Factorial where


-- | The naive but obviously correct implementation.
factorial_naive :: (Integral a) => a -> a
factorial_naive n
    | n < 0     = 0
    | otherwise = product [1..n]


-- | A common under-approximation of the factorial numbers.
factorial_stirling :: (Integral a) => a -> a
factorial_stirling n
    | n < 0     = 0
    | otherwise = ceiling (sqrt (2 * pi * n') * (n' / exp 1) ** n')
    where
    n' :: Double
    n' = fromIntegral n

{-
factorial_splitRecursive
factorial_primeSwing
factorial_parallelPrimeSwing
-}

----------------------------------------------------------------
----------------------------------------------------------- fin.