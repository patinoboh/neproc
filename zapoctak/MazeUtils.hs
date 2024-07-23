module MazeUtils (
    Maze,Cell(..),Point,printMaze,saveMaze,initializeMaze,setCell,
    inBounds,movements,applyMove,hasCycle,
    isValidMove,directions, generatePathPoints,setFree,setFree2
) where


import Data.Array.Base (writeArray)
import Data.Array.IO
import Data.Array.MArray
import Data.List (intercalate, nub)
import Data.Array

data Cell = Wall | Free
    deriving (Eq)

type Maze = [[Cell]]

instance Show Cell where 
    show Wall = "█"
    show Free = " "

type Point = (Int, Int)

printMaze :: Maze -> IO ()
printMaze = mapM_ (putStrLn . concatMap show)

mazeToString :: Maze -> String
mazeToString maze = intercalate "\n" $ map (concatMap show) maze

saveMaze :: FilePath -> Maze -> IO ()
saveMaze path maze = writeFile path $ mazeToString maze

initializeMaze :: Int -> Int -> Maze
initializeMaze height width = replicate height (replicate width Wall)

height :: Maze -> Int
height = length

width :: Maze -> Int
width = length . head



setCell :: Maze -> Point -> Cell -> Maze
setCell maze (x, y) cell
    | x < 0 || x >= length maze = maze  -- Check row index bounds
    | y < 0 || y >= length (head maze) = maze  -- Check column index bounds
    | otherwise = before ++ [newRow] ++ after
  where
    (before, row:after) = splitAt x maze
    (rowBefore, _:rowAfter) = splitAt y row
    newRow = rowBefore ++ (cell : rowAfter)


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

setFree :: [Point] -> Maze -> Maze
setFree crossroads maze = foldr (\point accMaze -> setCell accMaze point Free) maze points
    where 
        points = generatePathPoints crossroads

setFree2 :: [Point] -> Maze -> Maze
setFree2 crossroads maze = foldr (\point accMaze -> setCell accMaze point Free) maze crossroads

inBounds :: Int -> Int -> Point -> Bool
inBounds height width (x, y) = x >= 0 && x < height && y >= 0 && y < width

movements :: [Point]
movements = [(0, 1), (1, 0), (0, -1), (-1, 0)]

directions :: [Point]
directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]

applyMove :: Point -> Point -> Point
applyMove (x, y) (dx, dy) = (x + dx, y + dy)

isValidMove :: Maze -> [Point] -> Point -> Bool
isValidMove maze path (x, y) =
    inBounds (length maze) (length (head maze)) (x, y) &&
    notElem (x, y) path &&
    (maze !! x !! y == Wall)

hasCycle :: [Point] -> Bool
hasCycle points = length path /= length (nub path)
    where path = generatePathPoints points