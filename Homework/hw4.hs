import Control.Arrow (ArrowChoice(right))
import Data.ByteString (sort)
-- 4. úloha
--
-- 1) Implementujte kódování a dekódování RLE (https://en.wikipedia.org/wiki/Run-length_encoding):

-- >>> rleEncode "hello"
-- [(1,'h'),(1,'e'),(2,'l'),(1,'o')]
--
rleEncode :: (Eq a) => [a] -> [(Int, a)]
rleEncode [] = []
rleEncode (x:xs) = (number, x) : rleEncode cutted
    where
        number = length $ takeWhile (== x) (x:xs)
        cutted = dropWhile (== x) xs


-- >>> rleDecode [(1,'h'),(1,'e'),(2,'l'),(1,'o')]
-- "hello"
--
rleDecode :: [(Int, a)] -> [a]
rleDecode [] = []
rleDecode ((n, x):xs) = replicate n x ++ rleDecode xs


-- 2) Definujte nekonečný seznam všech prvočísel. Pokuste se o efektivní řešení.
-- Poté pomocí něj definujte funkci, která v daném rozsahu najde dvojici po sobě
-- jdoudích prvočísel s maximálním rozdílem. Pokud je jich více, vrátí první z nich.

-- >>> take 5 primes
-- [2,3,5,7,11]
--
primes :: [Integer]
primes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]


-- >>> gap 1000
-- (887, 907)


gap' :: [Integer] -> (Integer, Integer)
gap' []         = (0,0)
gap' [_]        = (0,0)
gap' (x1:x2:xs) = go (x1,x2) (x2:xs) where
    go largest [] = largest
    go largest [_] = largest
    go (m1, m2) (y1:y2:ys)
        | m2-m1 >= y2-y1 = go (m1,m2) (y2:ys)
        | otherwise      = go (y1,y2) (y2:ys)

gap :: Integer -> (Integer, Integer)
gap n = gap' (takeWhile (< n) primes)


--
-- >>> rleEncode (map gap [3 .. 1000])
-- [(2,(2,3)),(6,(3,5)),(18,(7,11)),(68,(23,29)),(30,(89,97)),(414,(113,127)),(366,(523,541)),(94,(887,907))]
--


-- Prvním argumentem je konec rozsahu, začátek bude vždy 2. Můžete předpokládat,
-- že konec bude alespoň 3.

-- 3) Implementujte mergesort, který vyhazuje duplikáty.

mergeWith :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeWith _ [] ys = ys
mergeWith _ xs [] = xs
mergeWith comparator (x:xs) (y:ys) = case comparator x y of
    LT -> x : mergeWith comparator xs (y:ys)
    GT -> y : mergeWith comparator (x:xs) ys
    EQ -> y : mergeWith comparator xs ys




sortWith :: (a -> a -> Ordering) -> [a] -> [a]
sortWith _ [] = []
sortWith _ [x] = [x]
sortWith f xs = mergeWith f (sortWith f left) (sortWith f right)
    where
        (left, right) = splitAt (length xs `div` 2) xs




-- Prvním argumentem je funkce, která provádí porovnávání.
-- Ordering je datový typ, který obsahuje 3 konstanty: LT, EQ, GT
-- (less than, equal, greater than).
--
-- >>> sortWith compare [10,9..1]
-- [1,2,3,4,5,6,7,8,9,10]
--
-- >>> sortWith (flip compare) [10,9..1]
-- [10,9,8,7,6,5,4,3,2,1]
--
-- >>> sortWith compare [1,1,1]
-- [1]
--
-- BONUS)
--
-- Implementujte následující funkce:

-- combinations n x vygeneruje seznam všech kombinací délky n ze seznamu x.
-- Na pořadí kombinací ve výsledném seznamu nezáleží.
--
-- >>> combinations 2 "abcd"
-- ["ab","ac","ad","bc","bd","cd"]
--
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

-- permutations x vygeneruje seznam všech permutací. Na pořadí permutací ve
-- výsledném seznamu nezáleží.
--
-- >>> permutations "abc"
-- ["abc","bac","bca","acb","cab","cba"]
--
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [x:ps | x <- xs, ps <- permutations (remove x xs)]
    where
        remove _ [] = []
        remove z (y:ys)
            | z == y = ys
            | otherwise = y : remove z ys



-- Pomocí těchto funkcí definujte "variace" (občas najdete v české literatuře,
-- v angličtině pro to termín asi neexistuje): kombinace, kde záleží na pořadí
--
-- >>> variations 2 "abc"
-- ["ab","ba","ac","ca","bc","cb"]
--
variations :: Int -> [a] -> [[a]]
variations = undefined
