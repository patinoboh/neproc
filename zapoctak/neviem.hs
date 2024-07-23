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
    

tryNewPoint :: Bool -> Point -> [Point] -> [Int] -> Maybe Int
tryNewPoint True _ fullPath [] = Nothing
tryNewPoint True p@(x,y) fullPath (newY:rest)
    | (y /= newY) && not (hasCycle2 (newLine ++ fullPath)) = Just newY
    | otherwise                                           = tryNewPoint True p fullPath rest
    where
    newLine = filter (/=p) (generatePathPoints [p, (x, newY)])

tryNewPoint False _ fullPath [] = Nothing
tryNewPoint False p@(x,y) fullPath (newX:rest)
    | (x /= newX) && not (hasCycle2 (newLine ++ fullPath)) = Just newX
    | otherwise                                            = tryNewPoint False p fullPath rest
    where
    newLine = filter (/=p) (generatePathPoints [p, (newX, y)])

generatePath :: Point -> Point -> Int -> Int -> IO [Point]
generatePath (fromX, width) (fromY, height) startX = go [(startX, 0)] True
  where
    retr = 100

    go :: [Point] -> Bool -> Int -> IO [Point]
    go path isVertical 0 = do
        putStrLn "som na nule"
        case tryMoveToEdges width height path of
            Just finalPath -> return finalPath
            -- Nothing        -> go (tail path) (not isVertical) 1
            Nothing        -> return []
    go path True len = do
        -- let (x,y) = head path
        -- let fullPath = generatePathPoints path
        -- let (yBot, yTop) = findRange True fullPath (x,y) (fromY, height - 2)
        ys <- rngList yBot yTop retr
        putStr $ "len " ++ show len ++ " yBot, yTop " ++ show (yBot, yTop) ++ " len path " ++ show (length path) ++ "\n"
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
        -- case tryNewPoint True (head path) fullPath ys of            
        --     Just newPoint -> do
        --         result <- go ((x, newPoint):path) False (len - 1)
        --         if null result
        --             then go (tail path) False (len + 1)
        --             else return result                    
        --     Nothing -> go (tail path) False (len + 1)


    go path False len = do
        let (x,y) = head path
        let fullPath = generatePathPoints path
        let (xBot, xTop) = findRange False fullPath (head path) (fromX, width - 2)
        putStr $ "len " ++ show len ++ " xBot, xTop " ++ show (xBot, xTop) ++ " len path " ++ show (length path) ++ "\n"
        xs <- rngList xBot xTop retr
        case tryNewPoint False (head path) fullPath xs of
            Just newPoint -> do
                result <- go ((newPoint, y):path) True (len - 1)
                if null result
                    then go (tail path) True (len + 1)
                    else return result
            Nothing -> go (tail path) True (len + 1)


-- | retries == 0 = go (tail path) (not isVertical) (len + 1) retr
-- let (x, y) = head path
-- if isVertical
-- then do
--     nextY <- rng 0 (height - 1)
--     if nextY /= y && not (hasCycle ((x, nextY):path))
--         then go ((x, nextY):path) False (len - 1)
--         else go path False (len + 1) -- Retry if invalid move
-- else do
--     nextX <- rng 0 (width - 1)
--     if nextX /= x && not (hasCycle ((nextX, y):path))
--         then go ((nextX, y):path) True (len - 1)
--         else go path True (len + 1) -- Retry if invalid move

main :: IO ()
main = do
    let width = 15
    let height = 15
    let startX = 6
    let pathLength = 10
    path <- generatePath (0, width) (0, height) startX pathLength
    let maze = connectAllPoints path (initializeMaze height width)
    printMaze maze