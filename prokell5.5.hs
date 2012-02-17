import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr(ord 'a' + n)

-- |以下はガード節。
-- |以下の条件がtrueとなる最初にマッチした式を実行。
-- ここで、otherwiseは実はtrueの別名。
shift :: Int -> Char -> Char
shift n c
 | isLower c = int2let((let2int c + n) `mod` 26)
 | otherwise = c

-- Stringは[Char] (Charのリスト)の等しい
-- encode :: Int -> [Char] -> [Char]
encode :: Int -> String -> String
-- リスト内包表記を用いる場合
-- encode n xs = [shift n x | x <- xs]
-- encode :: Int -> (String -> String)とする場合
-- ここでの()は関数を返すことを明示している。
encode n = map (shift n) 


decode :: Int -> String -> String
decode n = encode (-n)

