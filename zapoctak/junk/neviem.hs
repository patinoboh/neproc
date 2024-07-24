import MazeUtils
import RandomUtils


import Debug.Trace
import System.Random
import Control.Monad (replicateM)
import Data.List (delete, minimumBy, nub)
import Data.Ord (comparing)


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

hasCycle2 :: Eq a => [a] -> Bool
hasCycle2 fullPath = length fullPath /= length (nub fullPath)

tryMoveToEdges :: Int -> Int -> [Point] -> Maybe [Point]
tryMoveToEdges width height path = tryEdges edgePoints
  where
    (x, y) = head path
    edgePoints = [(0, y), (width - 1, y), (x, 0), (x, height - 1)]

    tryEdges [] = Nothing
    tryEdges (e:es)
        | not (hasCycle (e:path)) = Just (e:path)
        | otherwise = tryEdges es


findRange :: Bool -> [Point] -> Point -> Point -> (Int, Int)
findRange True fullPath (x,y) (fromY, toY) = (maximum smaller, minimum bigger)
    where
        allY = [yy | (xx, yy) <- fullPath, xx == x] ++ [fromY, toY]        
        bigger = if null biggerTemp then [y] else biggerTemp where
            biggerTemp = filter (> y) allY
        smaller = if null smallerTemp then [y] else smallerTemp where
            smallerTemp = filter (< y) allY
findRange False fullPath (x,y) (fromX, toX) = (maximum leftX, minimum rightX)
    where
        allX = [xx | (xx, yy) <- fullPath, yy == y] ++ [fromX, toX]        
        rightX = if null rightXTemp then [x] else rightXTemp where
            rightXTemp = filter (> x) allX
        leftX = if null leftXTemp then [x] else leftXTemp where
            leftXTemp = filter (< x) allX
    
generatePath :: Point -> Point -> Point -> Int -> IO [Point]
generatePath (fromX, width) (fromY, height) (startX, startY) = go [(startX, startY)] True
  where
    retr = 100

    go :: [Point] -> Bool -> Int -> IO [Point]
    go path isVertical 0 = do
        putStrLn $ "0"
        case tryMoveToEdges width height path of
            Just finalPath -> return finalPath
            Nothing        -> return []
    go path True len = do
        ys <- rngList yBot yTop retr
        -- ys <- shuffleRange yBot yTop
        putStrLn $ show len ++ " horizontal"
        exploreYs ys
            where
                (x,y) = head path
                fullPath = generatePathPoints path
                (yBot, yTop) = findRange True fullPath (x,y) (fromY, height - 2)                
                exploreYs [] = do 
                    go (tail path) False (len + 1)
                exploreYs (new:rest) = do
                    let newLine = filter (/=(x, y)) (generatePathPoints [(x, y), (x, new)])                
                    if (y /= new) && not (hasCycle2 (newLine ++ fullPath))
                        then do
                            let newPath = (x, new) : path
                            result <- go ((x, new):path) False (len - 1)
                            if null result
                                then exploreYs rest  -- Try next option if current one fails
                                else return result
                        else exploreYs rest  -- Try next option if current one fails


    go path False len = do
        xs <- rngList xBot xTop retr
        -- xs <- shuffleRange xBot xTop
        putStrLn $ show len ++ " vertical"
        exploreXs xs
            where
                (x,y) = head path
                fullPath = generatePathPoints path
                (xBot, xTop) = findRange False fullPath (head path) (fromX, width - 2)
                exploreXs [] = do
                    go (tail path) True (len + 1)
                exploreXs (new:rest) = do
                    let newLine = filter (/=(x, y)) (generatePathPoints [(x, y), (new, y)])                
                    if (x /= new) && not (hasCycle2 (newLine ++ fullPath))
                        then do
                            let newPath = (new, y) : path
                            result <- go ((new, y):path) True (len - 1)
                            if null result
                                then exploreXs rest  -- Try next option if current one fails
                                else return result
                        else exploreXs rest  -- Try next option if current one fails

main :: IO ()
main = do
    let width = 20
    let height = 10
    let startX = 6
    let pathLength = 10
    path <- generatePath (0, width) (0, height) (startX, 0) pathLength
    let maze = connectAllPoints path (initializeMaze height width)
    print path
    printMaze maze
