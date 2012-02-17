
-- Practice 5.1
squareSum :: Int -> Int
squareSum n = sum [x^2 | x <- [1..n]]

-- Practice 5.2
replicate :: Int -> a -> [a]
replicate n v = [v | _ <- [1..n]]

-- Practice 5.3
-- 複数行書く場合はインデントレベルを合わせないと、コンパイルエラーとなる
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(a,b,c) |
 a <- [1..n], b <- [1..n], c <- [1..n], 
 a*a + b*b == c*c
 ]

-- (a <= b <= c) の制約を持たせたpyths関数
pyths2 :: Int -> [(Int, Int, Int)]
pyths2 n = [(a, b, c) |
 a <- [1..n], b <- [a..n], c <- [b..n],
 a*a + b*b == c*c
 ]

-- Practice 5.4
-- nのすべての約数のリストを返す
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

-- 完全数: 自分自身を除く約数の和が自身と等しい
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) - x == x]

-- Practice 5.7
-- 2つのリストの内積を求める
-- リストの長さが違う場合はzipの定義により、
-- 短いリスト分のタプルを作成して処理
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x, y) <- zip xs ys]


