{-# OPTIONS_GHC
    -Wall
    -fwarn-tabs
    -fno-warn-name-shadowing
    -fno-warn-incomplete-patterns
    -fno-warn-incomplete-uni-patterns
    #-}
----------------------------------------------------------------
--                                                    2026-09-28
-- |
-- Module      :  Math.Combinatorics.Exact.Primes
-- Copyright   :  Copyright (c) 2011--2026 wren gayle romano
-- License     :  BSD
-- Maintainer  :  wren@cpan.org
-- Stability   :  experimental
-- Portability :  Haskell98
--
-- The prime numbers (<http://oeis.org/A000040>).
----------------------------------------------------------------
module Math.Combinatorics.Exact.Primes (primes) where

-- TODO: With the exception of the lists stored in a 'Wheel', all
-- the lists in this file are in fact infinite (modulo size issues
-- about 'Int').  Therefore it would be nice to implement (or find
-- on hackage) a datatype for infinite lists to avoid the cost of
-- unnecessary branches in case analysis, and to avoid the need for
-- `-fno-warn-incomplete-patterns` and `-fno-warn-incomplete-uni-patterns`.
-- In particular, we need the monad\/list-comprehension and @(++)@;
-- which alas seems to indicate that we need to use finite-lists
-- intermediately to constructing the infinite lists, unless we can
-- be especially clever.

data Wheel = Wheel {-# UNPACK #-}!Int ![Int]

-- BUG: the CAF is nice for sharing, but what about when we want
-- fusion and to avoid sharing? Using "Data.IntList" seems to only
-- increase the overhead. I guess things aren't being memoized/freed
-- like they should...

-- | The prime numbers. Implemented with the algorithm in:
--
-- * Colin Runciman (1997)
--    /Lazy Wheel Sieves and Spirals of Primes/, Functional Pearl,
--    Journal of Functional Programming, 7(2). pp.219--225.
--    ISSN 0956-7968
--    <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.55.7096>
--    TODO: get a new url for the paper, since citeseer is dead.
--
primes :: [Int]
primes = seive wheels primes primeSquares
    where
    primeSquares :: [Int]
    primeSquares = [p*p | p <- primes]

    wheels :: [Wheel]
    wheels = Wheel 1 [1] : zipWith nextSize wheels primes
        where
        nextSize :: Wheel -> Int -> Wheel
        nextSize (Wheel s ns) p =
            Wheel (s*p) [n' | o  <- [0,s..(p-1)*s]
                            , n  <- ns
                            , let n' = n+o
                            , n' `mod` p > 0 ]

    -- NOTE: I've switched to using lazy-patterns in lieu of 'head'
    -- and 'tail' in order to silence warnings on GHC >= 9.10.
    -- However, beware the syntax problems of combining as-patterns
    -- with lazy-patterns on GHC >= 9.0:
    -- <https://stackoverflow.com/q/67972231>
    -- <https://gitlab.haskell.org/ghc/ghc/-/wikis/migration/9.0#whitespace-sensitive----and->
    --
    -- Also note that `-fno-warn-incomplete-patterns` is no longer
    -- sufficient to silence the errors about incompete patterns here;
    -- we additionally need `-fno-warn-incomplete-uni-patterns`.
    -- Moreover, this isn't something we can resolve by simply expanding
    -- out the impossible cases, for some strange reason.

    seive :: [Wheel] -> [Int] -> [Int] -> [Int]
    -- NOTE: @pps@ and @qqs@ must be lazy; or else the circular program is _|_.
    seive (Wheel s ns : ws) pps@(~(p:ps)) qqs@(~(_:qs)) =
        [ n' | o  <- s : [2*s,3*s..(p-1)*s]
             , n  <- ns
             , let n' = n+o
             , s <= 2 || noFactorIn pps qqs n' ]
        ++ seive ws ps qs
        where
        noFactorIn :: [Int] -> [Int] -> Int -> Bool
        noFactorIn (p:ps) (q:qs) x =
            q > x || x `mod` p > 0 && noFactorIn ps qs x

----------------------------------------------------------------
----------------------------------------------------------- fin.
