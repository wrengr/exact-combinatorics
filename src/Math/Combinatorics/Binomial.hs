{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
----------------------------------------------------------------
--                                                    2011.12.14
-- |
-- Module      :  Math.Combinatorics.Binomial
-- Copyright   :  Copyright (c) 2011 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  Haskell98
--
-- Binomial coefficients, aka the count of possible combinations.
-- For negative inputs, all functions return 0 (rather than throwing
-- an exception or using 'Maybe').
----------------------------------------------------------------
module Math.Combinatorics.Binomial where

import Data.List                    (foldl')
import Math.Combinatorics.Primes    (primes)

{-
<http://mathworld.wolfram.com/BinomialCoefficient.html>

Some identities, but not really material for RULES:
    n `choose` 0     = 1
    n `choose` n     = 1
    n `choose` k     = n `choose` (n-k) when 0<=k<=n
    n `choose` k     = (-1)^k * ((k-n-1) `choose` k)
    n `choose` (k+1) = (n `choose` k) * ((n-k) / (k+1))
    (n+1) `choose` k = (n `choose` k) * (n `choose` (k-1))
    n `choose` j     = ((n-1)`choose` j) + ((n-1)`choose`(j-1)) when 0<j<n

Regarding the prime factorization/carries thing, also cf:
    Kummer (1852);
    Graham et al. (1989), Exercise 5.36, p. 245;
    Ribenboim (1989);
    Vardi (1991), p. 68

To extend to negative arguments and to complex numbers, see (Kronenburg 2011):
    n `choose` k
        | k >= 0    = (-1)^k     * ((-n+k-1) `choose` k)
        | k <= n    = (-1)^(n-k) * ((-k-1) `choose` (n-k))
        | otherwise = 0

According to Grinstead&Snell, p.95, when using the naive implementation
if you alternatete the multiplications and divisions then all
intermediate values are integers and none of the intermediate values
exceeds the final value. This property is retained in the fast
implementation.
-}


-- TODO: give a version that returns the prime-power factorization as [(Int,Int)]


-- | Exact binomial coefficients. The naive definition is:
--
-- > binomial n k
-- >     | k < 0     = 0
-- >     | k > n     = 0
-- >     | otherwise = factorial n / (factorial k * factorial (n-k))
--
-- However, we use a fast implementation based on the prime-power
-- factorization of the result (Goetgheluck, 1987). Each time @n@
-- is larger than the previous calls, there will be some slowdown
-- as the prime numbers must be computed (though it is still much
-- faster than the naive implementation); however, subsequent calls
-- will be extremely fast, since we memoize the list of 'primes'.
-- Do note, however, that this will result in a space leak if you
-- call @binomial@ for an extremely large @n@ and then don't need
-- that many primes in the future. Hopefully future versions will
-- correct this issue.
--
-- * P. Goetgheluck (1987)
--    /Computing Binomial Coefficients/,
--    American Mathematical Monthly, 94(4). pp.360--365.
--    <http://www.jstor.org/stable/2323099>,
--    <http://dl.acm.org/citation.cfm?id=26272>
--
binomial :: (Integral a) => a -> a -> a
    -- The result type could be any (Num b) if desired.
{-# SPECIALIZE binomial ::
    Integer -> Integer -> Integer,
    Int -> Int -> Int
    #-}
binomial n k_
    | n  <  0   = 0 -- N.B., @binomial_naive 0 0 == 1@
    | k_ <= 0   = if k_ == 0 then 1 else 0
    | k_ >= n   = if k_ == n then 1 else 0
    | otherwise =
        foldl'
            (\acc prime -> step acc (fromIntegral prime))
            1
            (takeWhile (fromIntegral n >=) primes)
        -- BUG: 'takeWhile' isn't a good producer, so we shouldn't
        -- just @map fromIntegral@. But 'take' is a good producer,
        -- so why isn't 'takeWhile'?
    where
    -- TODO: is it faster to replace (`quot` 2) by (`shiftR` 1) ?
    -- TODO: since we know the second operand to quot/rem is positive,
    -- we should use quotInt/remInt directly to avoid the extra
    -- tests (if they're not optimized away); for Integer the only
    -- extra test is against 0, but maybe we should call
    -- quotInteger/remInteger directly too.
    k     = fromIntegral $! if k_ > n `quot` 2 then n - k_ else k_
    nk    = n - k
    sqrtN = floor (sqrt (fromIntegral n) :: Double) `asTypeOf` n

    step acc prime
        | prime > nk         = acc * prime
        | prime > n `quot` 2 = acc
        | prime > sqrtN      =
            if n `rem` prime < k `rem` prime
            then acc * prime
            else acc
        | otherwise = acc * go n k 0 1
        where
        go n' k' r p
            | n' > 0 =
                if n' `rem` prime < (k' `rem` prime) + r
                then go (n' `quot` prime) (k' `quot` prime) 1 $! p * prime
                else go (n' `quot` prime) (k' `quot` prime) 0 p
            | otherwise = p
        {- -- BENCH: apparently this is an unreliable optimization.
        | otherwise = acc * (prime ^ go n k 0 0)
        where
        go n' k' r p
            | n' > 0 =
                if n' `rem` prime < (k' `rem` prime) + r
                then go (n' `quot` prime) (k' `quot` prime) 1 $! p+1
                else go (n' `quot` prime) (k' `quot` prime) 0 p
            | otherwise = p `asTypeOf` acc
        -}


{-
-- | Fast binomial. From Peter Luschny 2010-02-01, LGPL 2.1 or CC-BY-SA 3.0.
binomial_parallel n k
    | k < 0  = 0
    | k > n  = 0
    | k == 0 = 1
    | k == n = 1
    | otherwise = do
        let fi     = 0
        let k'     = if k > n / 2 then n - k else k
        let nk     = n - k'
        let sqrtN  = floor (sqrt n)
        let primes = takeWhile (n >=) primes
        --
        forM primes $ \prime -> do
            when (prime > nk) $ do
                primes[fi++] := prime
                continue
            when (prime > n / 2) $ do
                continue
            when (prime > sqrtN) $ do
                when (n `mod` prime < k `mod` prime) $ do
                    primes[fi++] := prime
                continue
            let r  = 0
            let n' = n
            let k'' = k'
            let p = 1
            while (n' > 0) $ do
                r := (n' `mod` prime) < (k'' `mod` prime + r) ? 1 : 0
                when (r==1) (p :*= prime)
                n'  :/= prime
                k'' :/= prime
            when (p > 1) (primes[fi++] := p)
        --
        productOf primes 0 fi

-- We only do the first part (converting Int to Long) if the array is of ints.
-- This builds @b@ in a way that ensures that operands are of approximately equal size (because that's faster), but the recursive call doesn't. Why?
productOf _ _ 0 = 1
productOf a start length =
    len := (length + 1) / 2
    b := new long[len]
    for (int k := 0, int i := start, int j := start + length - 1
        ; i < j
        ; ++i, ++j, --k
        )
        b[k] := a[i] * (long)a[j]
    if (i == j) b[k++] := a[j]
    if (k > PARALLEL_THRESHOLD)
        then
            future <- recProduct b ((k - 1) / 2 + 1) (k - 1)
            left =  recProduct b 0 ((k - 1) / 2)
            right <- future
            return (left * right)
        else
            recProduct b 0 (k-1)

recProduct arr n m
    | n >  m    = 1
    | n == m    = arr !! n
    | otherwise =
        let k = (n + m) `shiftR` 1
        in recProduct s n k * recProduct s (k + 1) m
-}

----------------------------------------------------------------
----------------------------------------------------------- fin.
