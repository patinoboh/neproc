import MazeUtils
import RandomUtils

import System.Random
import Control.Monad (replicateM)
import Data.List (delete, minimumBy, nub)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Function (on)

generatePathPoints :: [Point] -> [Point]
generatePathPoints [] = []
generatePathPoints [p] = [p]
generatePathPoints (p1@(x1, y1):p2@(x2, y2):rest)
    | x1 == x2  = [(x1, y) | y <- range y1 y2] ++ generatePathPoints (p2:rest)
    | y1 == y2  = [(x, y1) | x <- range x1 x2] ++ generatePathPoints (p2:rest)
    | otherwise = error "Path should only move horizontally or vertically"
    where
    range a b
        | a <= b    = [a..b-1]
        | otherwise = [b+1..a]

hasCycle :: [Point] -> Bool
hasCycle points = length path /= length (nub path)
    where path = generatePathPoints points


tryMoveToEdges :: Int -> Int -> [Point] -> Maybe [Point]
tryMoveToEdges width height path = tryEdges edgePoints
  where
    (x, y) = head path
    edgePoints = [(0, y), (width - 1, y), (x, 0), (x, height - 1)]

    tryEdges [] = Nothing
    tryEdges (e:es)
        | not (hasCycle (e:path)) = Just (e:path)
        | otherwise = tryEdges es

generatePath :: Int -> Int -> Int -> Int -> IO [Point]
generatePath width height startX pathLength = go [(startX, 0)] True pathLength retr
  where
    retr = 1000
    -- go path isVertical pathLength retries
    go :: [Point] -> Bool -> Int -> Int -> IO [Point]
    go path _ 0 _ = -- return path
        case tryMoveToEdges width height path of
            Just finalPath -> return finalPath
            Nothing        -> go (tail path) True 1 retr
    go path isVertical len retries
        | retries == 0 = go (tail path) (not isVertical) (len + 1) retr
        | otherwise = do
            let (x, y) = head path
            if isVertical
            then do
                nextY <- rng 0 (height - 1)
                if nextY /= y && not (hasCycle ((x, nextY):path))
                    then go ((x, nextY):path) False (len - 1) retr
                    else go path False (len + 1) (retries - 1) -- Retry if invalid move
            else do
                nextX <- rng 0 (width - 1)
                if nextX /= x && not (hasCycle ((nextX, y):path))
                    then go ((nextX, y):path) True (len - 1) (retries - 1)
                    else go path True (len + 1) retr -- Retry if invalid move


-- isInteriorFilledWithWalls :: [[Cell]] -> Point -> Point -> Bool
-- isInteriorFilledWithWalls maze (x1, y1) (x2, y2) =
--     all (all (== Wall)) $ map (take (y2 - y1 - 1) . drop (y1 + 1)) rows
--   where
--     rows = take (x2 - x1 - 1) $ drop (x1 + 1) maze

-- -- Function to find the largest rectangle for each turning point
-- calculateRectangles :: Int -> Int -> [[Cell]] -> [Point] -> [(Point, Point)]
-- calculateRectangles width height maze points = 
--     map (\p -> findLargestRectangle width height maze p) points

-- findLargestRectangle :: Int -> Int -> [[Cell]] -> Point -> (Point, Point)
-- findLargestRectangle width height maze point@(px, py) =
--     maximumBy (compare `on` area) validRectangles
--   where
--     validRectangles = [ ((px, py), (x2, y2)) |
--         x1 <- [0..px], x2 <- [px..width-1],
--         y1 <- [0..py], y2 <- [py..height-1],
--         x1 <= x2, y1 <= y2,
--         (x1, y1) /= (px, py),
--         isInteriorFilledWithWalls maze (x1, y1) (x2, y2)
--         ]
--     area ((x1, y1), (x2, y2)) = (x2 - x1 + 1) * (y2 - y1 + 1)

-- Utility function to get the maximum element based on a comparator
maximumBy :: (a -> a -> Ordering) -> [a] -> a
maximumBy _ [] = error "empty list"
maximumBy cmp (x:xs) = foldl (\acc y -> if cmp y acc == GT then y else acc) x xs


main :: IO ()
main = do
    let width = 15
    let height = 15
    let startX = 6
    let pathLength = 4
    path <- generatePath width height startX pathLength
    let maze = connectAllPoints path (initializeMaze height width)
    printMaze maze