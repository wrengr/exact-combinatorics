{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
----------------------------------------------------------------
--                                                    2011.12.14
-- |
-- Module      :  Math.Combinatorics.BinomialBench
-- Copyright   :  Copyright (c) 2011 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  Haskell98
--
----------------------------------------------------------------
module Math.Combinatorics.BinomialBench where
import Data.List                    (foldl')
import Math.Combinatorics.Primes    (primes)
import Math.Combinatorics.Factorial (factorial_naive)

import qualified Criterion.Main  as C
import qualified Control.DeepSeq as D (rnf)
----------------------------------------------------------------

-- TODO: now, how can we get these results into a database format
-- for plotting as a 3+1D (n,k,time;algorithm) graph?
--
-- | Hopefully the abstraction of breaking out and grouping the
-- tests won't affect the results...
main :: IO ()
main =
    case D.rnf $ takeWhile (maximum ns >=) primes of
    () -> C.defaultMain . for ns $ \n ->
            C.bgroup (show n) . for (getKs n) $ \k ->
                C.bgroup (show k)
                    -- See bug note.
                    -- [ C.bench "binomial_0" (C.whnf (binomial_0 n) k)
                    [ C.bench "binomial_1" (C.whnf (binomial_1 n) k)
                    , C.bench "binomial_2" (C.whnf (binomial_2 n) k)
                    , C.bench "binomial_3" (C.whnf (binomial_3 n) k)
                    , C.bench "binomial_4" (C.whnf (binomial_4 n) k)
                    ]
    where
    for = flip map
    
    ns :: [Int]
    {-# INLINE ns #-}
    ns =
        [ 1000
        , 5000
        , 10000
        , 50000
        , 100000
        , 500000
        , 1000000
        ]
    
    -- Rounded log_{1.5} spread: [1, 1.5, 2, 3, 5, 7, 10]
    {-# INLINE getKs #-}
    getKs n = map ($n)
        [ (`div` 100)
        -- , (`div` 70)
        -- , (`div` 50)
        , (`div` 30)
        -- , (`div` 20)
        , (`div` 15)
        -- , (`div` 10)
        , (`div` 5)
        -- , (`div` 3)
        , (`div` 2)
        ]


----------------------------------------------------------------
----------------------------------------------------------------

-- BUG: this is giving us a divide-by-zero error when compiled for
-- Criterion, but not when interpreted by Criterion, nor when run
-- standalone... wtf?
--
-- | The naive implementation: @n! / (k! * (n-k)!)@.
binomial_0 :: (Integral a) => a -> a -> a
{-# SPECIALIZE binomial_0 ::
    Integer -> Integer -> Integer,
    Int -> Int -> Int
    #-}
binomial_0 n k
    | k < 0     = 0
    | k > n     = 0
    | otherwise =
        factorial_naive n `quot` (factorial_naive k * factorial_naive (n-k))


-- TODO: a memoizing implementation based on pascal's triangle? cf
-- <http://www.polyomino.f2s.com/david/haskell/hs/CombinatoricsCounting.hs.txt>


-- | The starting efficient implementation.
binomial_1 :: (Integral a) => a -> a -> a
    -- The result type could be any (Num b) if desired.
{-# SPECIALIZE binomial_1 ::
    Integer -> Integer -> Integer,
    Int -> Int -> Int
    #-}
binomial_1 n k_
    | k_ <  0   = 0
    | k_ >  n   = 0
    | k_ == 0   = 1
    | k_ == n   = 1
    | otherwise =
        foldl'
            (\acc prime -> step acc (fromIntegral prime))
            1
            (takeWhile (fromIntegral n >=) primes)
    where
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


-- | Oddly, at -O1, factoring out the basis conditions makes it slower for
-- small @n@ with extremal @k@, though it gets faster for large
-- @n@. The differences are marginal in any case (+/- 1--3%). With -O2 this starts reliably outperforming 'binomial_1', however the difference is still marginal.
binomial_2 :: (Integral a) => a -> a -> a
    -- The result type could be any (Num b) if desired.
{-# SPECIALIZE binomial_2 ::
    Integer -> Integer -> Integer,
    Int -> Int -> Int
    #-}
binomial_2 n k_
    | k_ <= 0   = if k_ == 0 then 1 else 0
    | k_ >= n   = if k_ == n then 1 else 0
    | otherwise =
        foldl'
            (\acc prime -> step acc (fromIntegral prime))
            1
            (takeWhile (fromIntegral n >=) primes)
    where
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


-- | This seems on par with @binomial_1@ for small @n@, but scales
-- better. This is expected since exponentiation by '(^)' only
-- requires /O(log_2 p)/ multiplications instead of /O(p)/, however
-- it also requires /O(log_2 p)/ halvings of the exponent. There's
-- a lot more noise in the timings than expected, so it's unclear
-- whether the benefit outweighs the cost. Also, there's regression
-- against @binomial_2@ in certain cases. Again, the differences
-- appear to be marginal overall, so maybe it requires extremely
-- large @n@ before the cost of multiplications begins to dominate.
binomial_3 :: (Integral a) => a -> a -> a
    -- The result type could be any (Num b) if desired.
{-# SPECIALIZE binomial_3 ::
    Integer -> Integer -> Integer,
    Int -> Int -> Int
    #-}
binomial_3 n k_
    | k_ <  0   = 0
    | k_ >  n   = 0
    | k_ == 0   = 1
    | k_ == n   = 1
    | otherwise =
        foldl'
            (\acc prime -> step acc (fromIntegral prime))
            1
            (takeWhile (fromIntegral n >=) primes)
    where
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
        | otherwise = acc * (prime ^ go n k 0 0)
        where
        go n' k' r p
            | n' > 0 =
                if n' `rem` prime < (k' `rem` prime) + r
                then go (n' `quot` prime) (k' `quot` prime) 1 $! p+1
                else go (n' `quot` prime) (k' `quot` prime) 0 p
            | otherwise = p :: Int -- `asTypeOf` acc


-- | Combine the changes of @binomial_2@ and @binomial_3@.
binomial_4 :: (Integral a) => a -> a -> a
    -- The result type could be any (Num b) if desired.
{-# SPECIALIZE binomial_4 ::
    Integer -> Integer -> Integer,
    Int -> Int -> Int
    #-}
binomial_4 n k_
    | k_ <= 0   = if k_ == 0 then 1 else 0
    | k_ >= n   = if k_ == n then 1 else 0
    | otherwise =
        foldl'
            (\acc prime -> step acc (fromIntegral prime))
            1
            (takeWhile (fromIntegral n >=) primes)
    where
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
        | otherwise = acc * (prime ^ go n k 0 0)
        where
        go n' k' r p
            | n' > 0 =
                if n' `rem` prime < (k' `rem` prime) + r
                then go (n' `quot` prime) (k' `quot` prime) 1 $! p+1
                else go (n' `quot` prime) (k' `quot` prime) 0 p
            | otherwise = p :: Int -- `asTypeOf` acc

{-
binomial_z :: (Integral a) => a -> a -> a
    -- The result type could be any (Num b) if desired.
{-# SPECIALIZE binomial_z ::
    Integer -> Integer -> Integer,
    Int -> Int -> Int
    #-}
binomial_z n k_
    | k_ <  0   = 0
    | k_ >  n   = 0
    | k_ == 0   = 1
    | k_ == n   = 1
{-
-- TODO: benchmark whether factoring these four cases actually helps
-- in practice.
    | k_ <= 0   = if k_ == 0 then 1 else 0
    | k_ >= n   = if k_ == n then 1 else 0
-}
    | otherwise =
        foldl'
            (\acc prime -> step acc (fromIntegral prime))
            1
            (takeWhile (fromIntegral n >=) primes)
        -- BUG: takeWhile isn't a good producer, so we shouldn't
        -- just (map fromIntegral). But take is a good producer,
        -- so why isn't takeWhile?
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
        {-
        -- TODO: is this any faster? Need some solid benchmarks
        -- that ignore the overhead of generating the primes. It
        -- should be faster since it only uses O(log_2 p) multiplications
        -- instead of O(p), but maybe there's more overhead or something?
        | otherwise = acc * (prime ^ go n k 0 0)
        where
        go n' k' r p
            | n' > 0 =
                if n' `rem` prime < (k' `rem` prime) + r
                then go (n' `quot` prime) (k' `quot` prime) 1 $! p+1
                else go (n' `quot` prime) (k' `quot` prime) 0 p
            | otherwise = p `asTypeOf` acc
        -- -}
-}

----------------------------------------------------------------
----------------------------------------------------------- fin.
