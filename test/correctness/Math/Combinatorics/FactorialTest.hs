{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
----------------------------------------------------------------
--                                                    2012.01.27
-- |
-- Module      :  Math.Combinatorics.FactorialTest
-- Copyright   :  Copyright (c) 2011 wren gayle romano
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  Haskell98
--
----------------------------------------------------------------
module Math.Combinatorics.FactorialTest where
import Math.Combinatorics.Factorial (factorial)

import qualified Test.QuickCheck as QC
import qualified Test.SmallCheck as SC

----------------------------------------------------------------

main :: IO ()
main = do
    SC.smallCheck 25 prop_factorial
    -- QC.quickCheck prop_factorial -- Need a better generator, or something

----------------------------------------------------------------

-- | The naive but obviously correct implementation.
factorial_naive :: Int -> Integer
factorial_naive n
    | n < 0     = 0
    | otherwise = product [1 .. fromIntegral n]

prop_factorial :: Int -> Bool
prop_factorial n = factorial_naive n == factorial n

----------------------------------------------------------------
----------------------------------------------------------- fin.
