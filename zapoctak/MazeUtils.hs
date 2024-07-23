module MazeUtils (
    Maze,Cell(..),Point,printMaze,saveMaze,initializeMaze,setCell,
    connectPoints,connectAllPoints,inBounds,movements,applyMove,
    isValidMove,directions
) where


import Data.Array.Base (writeArray)
import Data.Array.IO
import Data.Array.MArray
import Data.List (intercalate)
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

setCell :: Maze -> Point -> Cell -> Maze
setCell maze (x, y) cell = 
    let (before, row:after) = splitAt x maze
        (rowBefore, _:rowAfter) = splitAt y row
    in before ++ [rowBefore ++ (cell : rowAfter)] ++ after

connectPoints :: Point -> Point -> Maze -> Maze
connectPoints (x1, y1) (x2, y2) maze
    | x1 == x2 = foldl (\m y -> setCell m (x1, y) Free) maze (range y1 y2)
    | y1 == y2 = foldl (\m x -> setCell m (x, y1) Free) maze (range x1 x2)
    | otherwise = foldl (\m (x, y) -> setCell m (x, y) Free) maze (zip (range x1 x2) (range y1 y2))
  where
    range a b
        | a <= b = [a..b]
        | otherwise = reverse [b..a]

connectAllPoints :: [Point] -> Maze -> Maze
connectAllPoints [] maze = maze
connectAllPoints [_] maze = maze
connectAllPoints (p1:p2:ps) maze = connectAllPoints (p2:ps) (connectPoints p1 p2 maze)


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
