-- 5. úloha
--
-- 1) Definujte datový typ 'Trie k v' reprezentující trii, kde klíče (řetězce)
-- jsou typu '[k]' a hodnoty typu 'v'.

data Trie k v = TODO

-- Implementujte následující:

empty :: Trie k v
empty = undefined

-- 'empty' je jednoduše konstanta, reprezentující prádznou trii.
--
-- > empty == fromList []
--

singleton :: [k] -> v -> Trie k v
singleton = undefined

-- 'singleton ks v' je trie, která obsahuje právě jednen klíč 'ks'
-- s hodnotou 'v'.
--
-- > singleton ks v == fromList [(ks, v)]
--

insertWith :: (Ord k) => (v -> v -> v) -> [k] -> v -> Trie k v -> Trie k v
insertWith = undefined

-- 'insertWith f ks new t' vloží klíč 'ks' s hodnotou 'new' do trie 't'. Pokud
-- trie již klíč 'ks' (s hodnotou 'old') obsahuje, původní hodnota je nahrazena
-- hodnotou 'f new old'.
--
-- > insertWith (++) "a" "x" empty                  == fromList [("a","x")]
-- > insertWith (++) "a" "x" (fromList [("a","y")]) == fromList [("a","xy")]
--

insert :: (Ord k) => [k] -> v -> Trie k v -> Trie k v
insert = undefined

-- 'insert ks new t' vloží klíč 'ks' s hodnotou 'new' do trie 't'. Pokud trie
-- již klíč 'ks' obsahuje, původní hodnota je nahrazena hodnotou 'new'
--
-- Hint: použijte 'insertWith'
--
-- > insert "a" "x" (fromList [("a","y")]) == fromList [("a","x")]
--

find :: (Ord k) => [k] -> Trie k v -> Maybe v
find = undefined

-- 'find k t' vrátí hodnotu odpovídající klíči 'k' (jako 'Just v'), pokud
-- existuje, jinak 'Nothing'.
--
-- > find "a" empty                  == Nothing
-- > find "a" (fromList [("a","x")]) == Just "x"
--

member :: (Ord k) => [k] -> Trie k v -> Bool
member = undefined

-- 'member k t' zjistí, jestli se klíč 'k' nalézá v trii 't'.
--
-- Hint: použijte 'find'
--
--
-- Funkce 'fromList' není nutná, ale může se vám hodit pro testování.

fromList :: (Ord k) => [([k], v)] -> Trie k v
fromList = undefined

-- BONUS) Implementujte funkci

delete :: (Ord k) => [k] -> Trie k v -> Trie k v
delete = undefined

-- 'delete ks t' smaže klíč 'ks' (a odpovídající hodnotu) z trie 't', pokud
-- klíč 'ks' není v trii obsažený, 'delete' vrátí původní trii.
--
-- > delete "a" (fromList [("b","y")]) == fromList [("b","y")]
-- > delete "a" (fromList [("a","x")]) == fromList []
