{-# OPTIONS_GHC
    -Wall
    -fwarn-tabs
    -fno-warn-incomplete-patterns
    -fno-warn-name-shadowing
    #-}
----------------------------------------------------------------
--                                                    2011.12.07
-- |
-- Module      :  Math.Combinatorics.Primes
-- Copyright   :  Copyright (c) 2011 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  Haskell98
--
-- The prime numbers (<http://oeis.org/A000040>).
----------------------------------------------------------------
module Math.Combinatorics.Primes (primes) where


data Wheel = Wheel {-# UNPACK #-}!Int ![Int]


-- BUG: the CAF is nice for sharing, but what about when we want fusion and to avoid sharing? Using Data.IntList seems to only increase the overhead. I guess things aren't being memoized/freed like they should...

-- | The prime numbers. Implemented with the algorithm in:
--
-- * Colin Runciman (1997)
--    /Lazy Wheel Sieves and Spirals of Primes/, Functional Pearl,
--    Journal of Functional Programming, 7(2). pp.219--225.
--    ISSN 0956-7968
--    <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.55.7096>
--
primes :: [Int]
primes = seive wheels primes primeSquares
    where
    primeSquares = [p*p | p <- primes]
    
    wheels = Wheel 1 [1] : zipWith nextSize wheels primes
        where
        nextSize (Wheel s ns) p =
            Wheel (s*p) [n' | o  <- [0,s..(p-1)*s]
                            , n  <- ns
                            , n' <- [n+o]
                            , n' `mod` p > 0 ]
    
    -- N.B., ps and qs must be lazy. Or else the circular program is _|_.
    seive (Wheel s ns : ws) ps qs =
        [ n' | o  <- s : [2*s,3*s..(head ps-1)*s]
             , n  <- ns
             , n' <- [n+o]
             , s <= 2 || noFactorIn ps qs n' ]
        ++ seive ws (tail ps) (tail qs)
        where
        -- noFactorIn :: [Int] -> [Int] -> Int -> Bool
        noFactorIn (p:ps) (q:qs) x =
            q > x || x `mod` p > 0 && noFactorIn ps qs x

----------------------------------------------------------------
----------------------------------------------------------- fin.