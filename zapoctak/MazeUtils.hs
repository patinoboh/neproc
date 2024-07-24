module MazeUtils (
    Maze,Cell(..),Point,printMaze,saveMaze,initializeMaze,setCell,
    hasCycle,printMazeTrue,
    generatePathPoints,setFree,setFree2
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

-- | Print maze to stdout
-- Prints it without postprocessing - for manipulation I work with bigger maze that must cropped afterwards. This one does not crop it.
printMazeTrue :: Maze -> IO ()
printMazeTrue = mapM_ (putStrLn . concatMap show)


-- | Print maze to stdout
-- Prints it with postprocessing - crops the maze
printMaze :: Maze -> IO ()
printMaze maze = mapM_ (putStrLn . concatMap' show) croppedRows
  where
    croppedRows = tail $ init maze
    concatMap' f = concatMap f . init . tail

-- | Convert maze to string
-- Converts maze without postprocessing - therefore I do not use it
mazeToString :: Maze -> String
mazeToString maze = intercalate "\n" $ map (concatMap show) maze

-- | Save maze to file
-- Saves maze without postprocessing - therefore I do not use it
saveMaze :: FilePath -> Maze -> IO ()
saveMaze path maze = writeFile path $ mazeToString maze

-- | Initialize maze with walls
initializeMaze :: Int -> Int -> Maze
initializeMaze height width = replicate height (replicate width Wall)

-- | Get height of the maze
height :: Maze -> Int
height = length

-- | Get width of the maze
width :: Maze -> Int
width = length . head


-- | Set cell in maze
setCell :: Maze -> Point -> Cell -> Maze
setCell maze (x, y) cell
    | x < 0 || x >= length maze = maze
    | y < 0 || y >= length (head maze) = maze
    | otherwise = before ++ [newRow] ++ after
  where
    (before, row:after) = splitAt x maze
    (rowBefore, _:rowAfter) = splitAt y row
    newRow = rowBefore ++ (cell : rowAfter)

-- | From list of crossroads generates all points on the path
-- The list must be a valid continuos path
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

-- | For a list of crossroads sets all cells to Free
setFree :: [Point] -> Maze -> Maze
setFree crossroads maze = foldr (\point accMaze -> setCell accMaze point Free) maze points
    where 
        points = generatePathPoints crossroads

-- | Sets only points in the list to free
setFree2 :: [Point] -> Maze -> Maze
setFree2 crossroads maze = foldr (\point accMaze -> setCell accMaze point Free) maze crossroads

-- | Check if the path (given as list of crossroads) has a cycle
hasCycle :: [Point] -> Bool
hasCycle points = length path /= length (nub path)
    where path = generatePathPoints points