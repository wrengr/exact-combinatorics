{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
-- {-# LANGUAGE CPP, MagicHash #-}
----------------------------------------------------------------
--                                                    2011.12.14
-- |
-- Module      :  Data.IntList
-- Copyright   :  Copyright (c) 2011 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  Haskell98 + FFI
--
-- A linked list of unboxed integer arrays.
----------------------------------------------------------------
module Data.IntList (IntList(), toIntList, fromIntList) where

import Foreign.Storable      (Storable(..), peekElemOff, pokeElemOff)
import Foreign.Ptr           (Ptr, nullPtr)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (mallocArray)
import System.IO.Unsafe      (unsafePerformIO)
{-
#ifdef __GLASGOW_HASKELL__
import GHC.Prim
#endif
-}
import GHC.Exts (build)
----------------------------------------------------------------

-- TODO: Do the ints automagically unbox, or shall we use Int# ?
-- TODO: is there any way we can actually unpack the array (not just the pointer) into Chunk? Or would that require using C structs?
data IntList
    = Chunk {-# UNPACK #-}!Int {-# UNPACK #-}!(Ptr Int) IntList


chunkSize :: Int
chunkSize = 256 -- TODO: choose this more intelligently


-- TODO: use pokeArraySplitAt?
toIntList :: [Int] -> IntList
toIntList xs0 = unsafePerformIO $ do
    ptr <- mallocArray chunkSize
    let go []         0      = do free ptr; return (Chunk 0 nullPtr undefined)
        go []         i      = return (Chunk i ptr (Chunk 0 nullPtr undefined))
        go xxs@(x:xs) i
            | i == chunkSize = return (Chunk i ptr (toIntList xxs))
            | otherwise      = do pokeElemOff ptr i x; go xs (i+1)
    --
    go xs0 0
    
{-
-- Implementation modified from base-4.4.1.0:Foreign.Marshal.Array.pokeArray
pokeArraySplitAt :: (Storable a) => Int -> Ptr a -> [a] -> IO [a]
#ifndef __GLASGOW_HASKELL__
pokeArraySplitAt size ptr xs0 =
    let (xs,ys) = splitAt size xs0
    in do zipWithM_ (pokeElemOff ptr) [0..] xs; return ys
#else
-- TODO: Is the unboxing of the index really necessary? Can't we let strictness analysis take care of that for us so we can eliminate the -XMagicHash ? dependency?
pokeArraySplitAt (I# size#) ptr xs0 = go xs0 0#
    where
    go []         _   = return []
    go xxs@(x:xs) i#
        | i# <# size# = do pokeElemOff ptr (I# i#) x; go xs (i# +# 1#)
        | otherwise   = return xxs
#endif
-}

fromIntList :: IntList -> [Int]
fromIntList = \xs0 -> build (builder xs0)
    where
    builder (Chunk size ptr xs0) cons nil
        | size <= 0 = nil
        | otherwise = unsafePerformIO $ go (size-1) (builder xs0 cons nil)
        where
        go 0 xs = do x <- peekElemOff ptr 0; return (cons x xs)
        go i xs = do x <- peekElemOff ptr i; go (i-1) (cons x xs)
{-
-- Implementation based on base-4.4.1.0:Foreign.Marshal.Array.peekArray
fromIntList :: IntList -> [Int]
fromIntList (Chunk size ptr xs0)
    | size <= 0 = []
    | otherwise = unsafePerformIO $ go (size-1) (fromIntList xs0)
    where
    go 0 xs = do x <- peekElemOff ptr 0; return (x:xs)
    go i xs = do x <- peekElemOff ptr i; go (i-1) (x:xs)
-}

----------------------------------------------------------------
----------------------------------------------------------- fin.