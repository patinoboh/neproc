import MazeUtils
import RandomUtils
import Data.List (nub)

twists :: Int -> Int -> Int -> Int -> Int -> IO [Point]
twists xFrom xTo yFrom yTo n = do
    let num = n `div` 2 - 1
    xCoords <- rngList xFrom xTo num
    yCoords <- rngList yFrom yTo (num + 1)
    return $ zip (0:concatMap (replicate 2) xCoords ++ [xTo + 1]) (concatMap (replicate 2) yCoords)

generateValidPath :: Int -> Int -> Int -> Int -> Int -> IO [Point]
generateValidPath xFrom xTo yFrom yTo numJunctions = do
    valid <- twists xFrom xTo yFrom yTo numJunctions
    let path = generatePathPoints valid
    if not (hasCycle path)
        then return path
        else generateValidPath xFrom xTo yFrom yTo numJunctions