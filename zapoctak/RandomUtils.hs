module RandomUtils(
    rng,
    rngList,
    shuffleRange,
    randomBool
) where

import System.Random
import Control.Monad (replicateM, forM_)
import System.Posix.Internals (puts)

-- | Generate random number in range [min, max] that is even
rng :: Int -> Int -> IO Int
rng min max = do
    x <- getStdRandom (randomR (min `div` 2, max `div` 2))
    return (x * 2)

-- | Generate random number in range [min, max]
rngNonEven :: Int -> Int -> IO Int
rngNonEven min max = getStdRandom (randomR (min, max))

-- | Generate list of n random even numbers in range [min, max]
rngList :: Int -> Int -> Int -> IO [Int]
rngList min max n = replicateM n (rng min max)

-- | Generate list of even numbers in range [min, max]
evenNumbersFromRange :: Int -> Int -> [Int]
evenNumbersFromRange min max = [x | x <- [min..max], even x]

-- | Shuffles the list of numbers
shuffle :: [Int] -> IO [Int]
shuffle xs = do shuffleHelper xs (length xs - 1)
  where
    shuffleHelper :: [Int] -> Int -> IO [Int]
    shuffleHelper [] _ = return []
    shuffleHelper ys n = do
        idx <- rngNonEven 0 n
        let (before, x:after) = splitAt idx ys
        rest <- shuffleHelper (before ++ after) (n - 1)
        return (x : rest)

-- | Shuffles the list of even numbers in range [min, max]
shuffleRange :: Int -> Int -> IO [Int]
shuffleRange min max = do
    let numbers = evenNumbersFromRange min max
    shuffle numbers

-- | Generate random boolean
randomBool :: IO Bool
randomBool = do
    randInt <- rngNonEven 0 1
    return (randInt == 1)