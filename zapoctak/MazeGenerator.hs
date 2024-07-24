import MazeUtils
import RandomUtils

import System.Environment (getArgs)

import Debug.Trace
import System.Random
import Control.Monad (replicateM)
import Data.List (delete, minimumBy, nub)
import Data.Ord (comparing)

import System.IO (hPutStrLn, stderr)

-- | Prints message into stdErr
-- I use this to print debug messages
printToStderr :: String -> IO ()
printToStderr = hPutStrLn stderr

-- | Checks if the path can be extended to an edge
tryMoveToEdges :: Int -> Int -> [Point] -> Maybe [Point]
tryMoveToEdges height width path = tryEdges edgePoints
  where
    (row, col) = head path
    edgePoints = [(0, col), (row, width), (row, 0), (height, col)]

    tryEdges [] = Nothing
    tryEdges (e:es)
        | not (hasCycle (e:path)) = Just (e:path)
        | otherwise = tryEdges es

-- | For a given point returns vertical or horizontal range in which the maze consists of walls. The point must be in this range
findRange :: Bool -> [Point] -> Point -> Point -> (Int, Int)
findRange True fullPath (row,col) (fromRow, toRow) = (maximum smaller, minimum bigger)
    where
        allY = [r | (r, c) <- fullPath, c == col] ++ [fromRow, toRow]
        bigger = if null biggerTemp then [row] else biggerTemp where
            biggerTemp = filter (> row) allY
        smaller = if null smallerTemp then [row] else smallerTemp where
            smallerTemp = filter (< row) allY
findRange False fullPath (row,col) (fromCol, toCol) = (maximum leftX, minimum rightX)
    where
        allX = [c | (r, c) <- fullPath, r == row] ++ [fromCol, toCol]
        rightX = if null rightXTemp then [col] else rightXTemp where
            rightXTemp = filter (> col) allX
        leftX = if null leftXTemp then [col] else leftXTemp where
            leftXTemp = filter (< col) allX
    

-- | Generates list of points (crossroads) where the direction of a path changes
-- Arguments : rowRange columnRange StartPoint numJunctions
-- The result might be one longer than expected due to the ending point
generatePath :: Point -> Point -> Point -> Int -> IO [Point]
generatePath (minRow, maxRow) (minCol, maxCol) (startRow, startCol) = go [(startRow, startCol)] True
  where
    retr = 100

    go :: [Point] -> Bool -> Int -> IO [Point]
    go path isVertical 0 = do
        case tryMoveToEdges (maxRow + 1) (maxCol + 1) path of
            Just finalPath -> return finalPath
            Nothing        -> return []
    go path True len = do
        -- ys <- rngList rowBottom rowTop retr
        ys <- shuffleRange (rowBottom + 1) (rowTop - 1)
        exploreYs ys
            where
                (row,col) = head path
                fullPath = generatePathPoints path
                (rowBottom, rowTop) = findRange True fullPath (row,col) (minRow, maxRow - 1)
                exploreYs [] = do 
                    go (tail path) False (len + 1)
                exploreYs (newRow:rest) = do
                    if row /= newRow
                        then do
                            let newPath = (newRow, col) : path
                            result <- go newPath False (len - 1)
                            if null result
                                then exploreYs rest
                                else return result
                    else exploreYs rest
    go path False len = do
        -- xs <- rngList colBottom colTop retr
        xs <- shuffleRange (colBottom + 1) (colTop - 1)        
        exploreXs xs
            where
                (row,col) = head path
                fullPath = generatePathPoints path
                (colBottom, colTop) = findRange False fullPath (head path) (minCol, maxCol - 1)
                exploreXs [] = do
                    go (tail path) True (len + 1)
                exploreXs (newCol:rest) = do
                    if col /= newCol
                        then do
                            let newPath = (row, newCol) : path
                            result <- go newPath True (len - 1)
                            if null result
                                then exploreXs rest
                                else return result
                    else exploreXs rest

-- | Generates a dead end
-- Arguments : rowRange columnRange StartPoint fullPath isVertical numJunctions
-- fullPath = [Point] of all free points in the maze
generateDeadEnd :: Point -> Point -> Point -> [Point] -> Bool -> Int -> IO [Point]
generateDeadEnd (minRow, maxRow) (minCol, maxCol) (startRow, startCol) fullMaze = go [(startRow, startCol)]
  where
    go :: [Point] -> Bool -> Int -> IO [Point]
    -- deadPath vertical len
    go deadPath _ 0 = return deadPath
    go deadPath True len = do
        yys <- shuffleRange (rowBottom + 1) (rowTop - 1)
        let ys = filter (/=row) yys
        exploreYs ys
            where
                (row,col) = head deadPath
                fullPath = generatePathPoints deadPath
                (rowBottom, rowTop) = findRange True (fullMaze ++ fullPath) (row,col) (minRow, maxRow - 1)
                exploreYs [] = return deadPath
                exploreYs (newRow:rest) = go ((newRow, col) : deadPath) False (len - 1)            
    
    go deadPath False len = do
        xxs <- shuffleRange (colBottom + 1) (colTop - 1)
        let xs = filter (/=col) xxs
        exploreXs xs
            where
                (row,col) = head deadPath
                fullPath = generatePathPoints deadPath
                (colBottom, colTop) = findRange False (fullMaze ++ fullPath) (row,col) (minCol, maxCol - 1)
                exploreXs [] = return deadPath
                exploreXs (newCol:rest) = go ((row, newCol) : deadPath) True (len - 1)


-- | Generates dead ends for all points in the list
-- Arguments : rowRange columnRange fullPath points numJunctions
-- fullPath = [Point] of all free points in the maze
-- points = [Point] of all crossroads in the maze
generateDeadEnds :: Point -> Point -> [Point] -> [Point] -> Int -> IO [Point]
generateDeadEnds _ _ f [] _ = return f
generateDeadEnds (minRow, maxRow) (minCol, maxCol) fullMazePath points@(start:rest) pathLength = do
    randBool <- randomBool
    deadEnd <- generateDeadEnd (minRow, maxRow) (minCol, maxCol) start fullMazePath randBool pathLength
    -- I might want to create both vertical and horizontal deadend
    let newMaze = fullMazePath ++ generatePathPoints deadEnd
    generateDeadEnds (minRow, maxRow) (minCol, maxCol) newMaze rest pathLength

interactiveMain :: IO ()
interactiveMain = do
    let height = 20
    let width = 30
    let startCol = 6
    let pathLength = 10
    -- print "Generating path..."
    printToStderr "Generating path..."
    path <- generatePath (0, height - 1) (0, width - 1) (0, startCol) pathLength
    -- print "Path generated!"
    print path
    let fullPath = generatePathPoints path
    print "Generating dead ends..."
    mazePath <- generateDeadEnds (0, height - 1) (0, width - 1) fullPath (tail (init path)) 10
    print "Dead ends generated!"
    let maze = setFree2 mazePath (initializeMaze height width)
    -- let maze = setFree path (initializeMaze height width)
    print path
    printMaze maze


main :: IO ()
main = do
    args <- getArgs
    let heightR = read (args !! 0) :: Int
    let height = heightR + 2
    let widthR = read (args !! 1) :: Int    
    let width = widthR + 2
    let startCol = read (args !! 2) :: Int
    let pathLength = read (args !! 3) :: Int
    let deadPathLength = read (args !! 4) :: Int
    
    -- print "Generating path..."
    printToStderr "Generating path..."

    path <- generatePath (0, height - 1) (0, width - 1) (0, startCol) pathLength
    
    -- print "Path generated!"
    printToStderr "Path generated!"
    
    let fullPath = generatePathPoints path

    -- print "Generating dead ends..."
    printToStderr "Generating dead ends..."

    mazePath <- generateDeadEnds (0, height - 1) (0, width - 1) fullPath (tail (init path)) deadPathLength

    -- print "Dead ends generated!"
    printToStderr "Dead ends generated!"

    let maze = setFree2 mazePath (initializeMaze height width)    
    printMaze maze
    -- saveMaze fileName maze
    -- print "Maze saved!"
    -- printToStderr "Maze saved!"
