module Lib where

import Control.Monad     (guard, (>=>))
import Control.Monad.ST  (ST)
import Data.Array.IArray (assocs)
import Data.Array.MArray (newArray, writeArray)
import Data.Array.ST     (STUArray, runSTUArray)
import Data.List         (foldl1')

triplesLessThan :: Int -> [Int]
triplesLessThan c = map fst . filter snd . assocs
                  $ runSTUArray (newArray (1,c) False >>= truifyHypoUntil c)

truifyHypoUntil :: Int -> STUArray s Int Bool -> ST s (STUArray s Int Bool)
truifyHypoUntil c = foldl1' (>=>) $ map (truifyMultiples c) (primitivesLessThan c)

truifyMultiples :: Int -> Int -> STUArray s Int Bool -> ST s (STUArray s Int Bool)
truifyMultiples c n = foldl1' (>=>) . map truify $ [n,2*n .. c]
  where
    truify :: Int -> STUArray s Int Bool -> ST s (STUArray s Int Bool)
    truify i a = do writeArray a i True; return a

primitivesLessThan :: Int -> [Int]
primitivesLessThan c = do
  n <- [1   .. nLimit c]
  m <- [n+1 .. mLimit c n]
  guard (gcd n m == 1)
  return $ m^2 + n^2

nLimit c   = floor . sqrt $ (fromIntegral c / 2)
mLimit c n = floor . sqrt $ (fromIntegral c - (fromIntegral n)^2)
