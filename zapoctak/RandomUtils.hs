module RandomUtils(
    rng,
    rngList,
    shuffleRange,
    randomBool
) where

import System.Random
import Control.Monad (replicateM, forM_)
import System.Posix.Internals (puts)

rng :: Int -> Int -> IO Int
rng min max = do
    x <- getStdRandom (randomR (min `div` 2, max `div` 2))
    return (x * 2)

rngNonEven :: Int -> Int -> IO Int
rngNonEven min max = getStdRandom (randomR (min, max))

rngList :: Int -> Int -> Int -> IO [Int]
rngList min max n = replicateM n (rng min max)


evenNumbersFromRange :: Int -> Int -> [Int]
evenNumbersFromRange min max = [x | x <- [min..max], even x]

fisherYatesShuffle :: [Int] -> IO [Int]
fisherYatesShuffle xs = do
    shuffled <- shuffleHelper xs (length xs - 1)
    return shuffled
  where
    shuffleHelper :: [Int] -> Int -> IO [Int]
    shuffleHelper [] _ = return []
    shuffleHelper ys n = do
        idx <- randomRIO (0, n)
        let (before, x:after) = splitAt idx ys
        rest <- shuffleHelper (before ++ after) (n - 1)
        return (x : rest)

shuffleRange :: Int -> Int -> IO [Int]
shuffleRange min max = do
    let numbers = evenNumbersFromRange min max
    fisherYatesShuffle numbers

randomBool :: IO Bool
randomBool = do
    randInt <- rngNonEven 0 1
    return (randInt == 1)