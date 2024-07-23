module RandomUtils(
    rng,
    rngList
) where

import System.Random
import Control.Monad (replicateM, forM_)

rng :: Int -> Int -> IO Int
rng min max = do
    x <- getStdRandom (randomR (min, max))
    if even x
        then return x
        else rng min max

rngList :: Int -> Int -> Int -> IO [Int]
rngList min max n = replicateM n (rng min max)
