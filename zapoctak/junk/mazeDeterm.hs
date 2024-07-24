import System.Random
import Control.Monad (replicateM, forM_)
import Data.Array.Base (writeArray)
import Data.Array.IO
import Data.Array.MArray
import Data.List (intercalate)
import Data.Array


data Cell = Wall | Free
    deriving (Eq)

type Maze = [[Cell]]

instance Show Cell where 
    show Wall = "#"
    show Free = " "

type Point = (Int, Int)

rng :: Int -> Int -> IO Int
rng min max = getStdRandom (randomR (min, max))

printMaze :: Maze -> IO ()
printMaze = mapM_ (putStrLn . concatMap show)

mazeToString :: Maze -> String
mazeToString maze = intercalate "\n" $ map (concatMap show) maze


height = length
width m = length (head m)

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


saveMaze :: FilePath -> Maze -> IO ()
saveMaze path maze = writeFile path $ mazeToString maze


getTurns :: Int -> Maze -> IO ([Int],[Int])
getTurns twists m = do
    yCoords <- replicateM (twists `div` 2 -1) (rng 1 (height m - 1))
    xCoords <- replicateM (twists `div` 2) (rng 1 (width m - 1))
    return (0:concatMap (replicate 2) yCoords ++ [height m], concatMap (replicate 2) xCoords)

    


-- main :: IO ()
-- main = do
--     let height = 10
--     let width = 10
--     maze <- initializeMaze height width







-- createPath :: Int -> Int -> Maze
-- createPath 
