import MazeUtils
import RandomUtils


import Debug.Trace
import System.Random
import Control.Monad (replicateM)
import Data.List (delete, minimumBy, nub)
import Data.Ord (comparing)
import CoreSyn (deAnnBind)


tryMoveToEdges :: Int -> Int -> [Point] -> Maybe [Point]
tryMoveToEdges height width path = tryEdges edgePoints
  where
    (row, col) = head path
    edgePoints = [(0, col), (row, width), (row, 0), (height, col)]

    tryEdges [] = Nothing
    tryEdges (e:es)
        | not (hasCycle (e:path)) = Just (e:path)
        | otherwise = tryEdges es


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
    
generatePath :: Point -> Point -> Point -> Int -> IO [Point]
-- generatePath (minRow, maxRow) (minCol, maxCol) (startRow, startCol) = go [(startRow, startCol), (1, startCol), (0, startCol)] True
generatePath (minRow, maxRow) (minCol, maxCol) (startRow, startCol) = go [(startRow, startCol)] True
  where
    retr = 100

    go :: [Point] -> Bool -> Int -> IO [Point]
    go path isVertical 0 = do
        -- putStrLn $ "0"
        case tryMoveToEdges (maxRow + 1) (maxCol + 1) path of
            Just finalPath -> return finalPath
            Nothing        -> return []
    go path True len = do
        -- ys <- rngList rowBottom rowTop retr
        ys <- shuffleRange (rowBottom + 1) (rowTop - 1)
        -- putStrLn $ show len ++ " V, (rB,rT)=(" ++ show rowBottom ++ "," ++ show rowTop ++ ") ys (min,max)=" ++ show (minimum ys) ++ "," ++ show (maximum ys) ++ ")"
        -- putStrLn $ "V, (rB,rT)=(" ++ show rowBottom ++ "," ++ show rowTop ++ ")"
        -- print ys
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
        -- putStrLn $ show len ++ " horizontal"
        -- putStrLn $ show len ++ " H, (cB,cT)=(" ++ show colBottom ++ "," ++ show colTop ++ ") xs (min,max)=" ++ show (minimum xs) ++ "," ++ show (maximum xs) ++ ")"
        -- putStrLn $ "H, (cB,cT)=(" ++ show colBottom ++ "," ++ show colTop ++ ")"
        -- print xs
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

generateDeadEnd :: Point -> Point -> Point -> [Point] -> Bool -> Int -> IO [Point]
-- (rozsah) (rozsah) (odkial) [celaCesta] vertical? dlzka
generateDeadEnd (minRow, maxRow) (minCol, maxCol) (startRow, startCol) fullMaze vertical pathLength = go [(startRow, startCol)] vertical pathLength
  where
    go :: [Point] -> Bool -> Int -> IO [Point]
    -- deadPath vertical len
    go deadPath _ 0 = return deadPath
    go deadPath True len = do
        ys <- shuffleRange (rowBottom + 1) (rowTop - 1)
        -- putStrLn $ show len ++ " D (rB,rT)=(" ++ show rowBottom ++ "," ++ show rowTop ++ ") ys=" ++ show ys ++ ")"
        -- putStrLn $ "full maze len : " ++ show (length fullMaze) ++ " full path len : " ++ show (length fullPath) ++ " dead path len : " ++ show (length deadPath) ++ " spolu : " ++ show (length (fullMaze ++ fullPath))
        exploreYs ys
            where
                (row,col) = head deadPath
                fullPath = generatePathPoints deadPath
                (rowBottom, rowTop) = findRange True (fullMaze ++ fullPath) (row,col) (minRow, maxRow - 1)
                exploreYs [] = return []
                exploreYs (newRow:rest) = do
                    if row /= newRow
                        then do
                            let newPath = (newRow, col) : deadPath
                            -- putStrLn $ "Calling go recursively from V len : " ++ show len
                            result <- go newPath False (len - 1)
                            if null result
                                then exploreYs rest
                                else return result
                    else exploreYs rest
    go deadPath False len = do
        xs <- shuffleRange (colBottom + 1) (colTop - 1)
        -- putStrLn $ show len ++ " D (rB,rT)=(" ++ show colBottom ++ "," ++ show colTop ++ ") ys=" ++ show xs ++ ")"
        exploreXs xs
            where
                (row,col) = head deadPath
                fullPath = generatePathPoints deadPath
                (colBottom, colTop) = findRange False (fullMaze ++ fullPath) (row,col) (minCol, maxCol - 1)
                exploreXs [] = return []
                exploreXs (newCol:rest) = do
                    if col /= newCol
                        then do
                            let newPath = (row, newCol) : deadPath
                            -- putStrLn $ "Calling go recursively from H len : " ++ show len
                            result <- go newPath True (len - 1)
                            if null result
                                then exploreXs rest
                                else return result
                    else exploreXs rest

generateDeadEnds :: Point -> Point -> [Point] -> [Point] -> Int -> IO [Point]
generateDeadEnds _ _ f [] _ = return f
generateDeadEnds (minRow, maxRow) (minCol, maxCol) fullMazePath points@(start:rest) pathLength = do
    randBool <- randomBool
    -- putStrLn $ "Generating dead end number : " ++ show (length points)
    deadEnd <- generateDeadEnd (minRow, maxRow) (minCol, maxCol) start fullMazePath randBool pathLength    
    print deadEnd
    let newMaze = fullMazePath ++ generatePathPoints deadEnd
    generateDeadEnds (minRow, maxRow) (minCol, maxCol) newMaze rest pathLength

-- main :: IO ()
-- main = do
--     let width = 30
--     let height = 20
--     let startCol = 6
--     let pathLength = 10
--     path <- generatePath (1, height - 1) (1, width - 1) (1, startCol) pathLength
--     let maze = setFree path (initializeMaze height width)
--     print path
--     printMaze maze


-- main :: IO ()
-- main = do
--     let height = 20
--     let width = 30
--     let startCol = 6
--     let pathLength = 10
--     print "Generating path..."
--     path <- generatePath (0, height - 1) (0, width - 1) (0, startCol) pathLength
--     print "Path generated!"
--     let maze = setFree path (initializeMaze height width)
--     printMaze maze


main :: IO ()
main = do
    let height = 20
    let width = 30
    let startCol = 6
    let pathLength = 10
    print "Generating path..."
    path <- generatePath (0, height - 1) (0, width - 1) (0, startCol) pathLength
    print "Path generated!"
    print path
    let fullPath = generatePathPoints path
    print "Generating dead ends..."
    mazePath <- generateDeadEnds (0, height - 1) (0, width - 1) fullPath (tail (init path)) 4
    print "Dead ends generated!"
    let maze = setFree2 mazePath (initializeMaze height width)
    -- let maze = setFree path (initializeMaze height width)
    print path
    printMaze maze