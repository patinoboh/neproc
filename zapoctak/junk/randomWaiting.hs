import MazeUtils
import System.Random
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.List

rng :: Int -> Int -> IO Int
rng min max = do
    x <- getStdRandom (randomR (min, max))
    if even x
        then return x
        else rng min max

rngList :: Int -> Int -> Int -> IO [Int]
rngList min max n = replicateM n (rng min max)

twists :: Int -> Int -> Int -> Int -> Int -> IO [Point]
twists xFrom xTo yFrom yTo n = do
    let num = n `div` 2 - 1
    xCoords <- rngList xFrom xTo num
    yCoords <- rngList yFrom yTo (num + 1)
    return $ zip (0:concatMap (replicate 2) xCoords ++ [xTo + 1]) (concatMap (replicate 2) yCoords)


-- Generate a path from the list of junctions
createPathFromJunctions :: [Point] -> [Point]
createPathFromJunctions (start:xs) = concatMap (\(a, b) -> [a, b]) (zip (start:xs) xs)
createPathFromJunctions [] = []

-- Ensure the path does not cross itself
validatePath :: [Point] -> Bool
validatePath path = length path == length (nub path)


generateValidPath :: Int -> Int -> Int -> Int -> Int -> IO [Point]
generateValidPath xFrom xTo yFrom yTo numJunctions = do
    valid <- twists xFrom xTo yFrom yTo numJunctions
    let path = createPathFromJunctions valid
    if validatePath path
        then return path
        else generateValidPath xFrom xTo yFrom yTo numJunctions



-- findPathDFS :: Maze -> Point -> Point -> Int -> [Point] -> IO [Point]
-- findPathDFS maze start end maxBends path
--     | length path > maxBends = return [] -- Path length exceeds allowed bends
--     | start == end = return path -- Path successfully found
--     | otherwise = do
--         -- Try each possible movement
--         let possibleMoves = [applyMove start move | move <- directions]
--         let validMoves = filter (isValidMove maze path) possibleMoves
--         -- Explore each valid move recursively
--         results <- mapM (\move -> findPathDFS maze move end maxBends (start : path)) validMoves
--         return $ head (filter (not . null) results) -- Return first valid path or empty list


-- Apply a move to a point
applyMove :: Point -> Point -> Point
applyMove (x, y) (dx, dy) = (x + dx, y + dy)


main :: IO ()
main = do
    let height = 21
    let width = 21
    let numJunctions = 6
    let maze = initializeMaze height width
    print "Looking for path"
    path <- generateValidPath 1 (height - 2) 1 (width - 2) numJunctions
    print "Path found!"
    let mazeWithPath = connectAllPoints path maze
    print mazeWithPathc