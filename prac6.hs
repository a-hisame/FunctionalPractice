
-- prac 6.8.1
pow :: Int -> Int -> Int
pow _ 0 = 1
pow n k = n * pow n (k-1)

-- prac 6.8.3
-- and is already defined.
allt :: [Bool] -> Bool
allt [] = False
allt (x:[]) = x
allt (False:_) = False
allt (True:xs) = and xs

-- myconcat
myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ (myconcat xs) 

-- myreplicate
myreplicate :: Int -> a -> [a]
myreplicate 0 x = []
myreplicate n x = x : (myreplicate (n-1) x)

-- index (!!)
-- index "abc" 0 is 'a'
index :: [a] -> Int -> Maybe a
index [] _ = Nothing
index (x:xs) 0 = Just x
index (_:xs) n = index xs (n-1)

-- exists (elem)
-- | means 'when' in OCaml
exists :: Eq a => a -> [a] -> Bool
exists _ [] = False
exists v (x:_) | (x == v) = True
exists v (_:xs) = exists v xs

-- merge 
-- 1st list and 2nd list need sorted.
-- xxs@(x:xs) means xxs = x:xs
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xxs@(x:xs) yys@(y:ys) = 
 if x < y 
  then x : (merge xs yys) 
  else y : (merge xxs ys)

-- halve (divide list)
-- fst list size greater than equals snd list size
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
 where n = div (length xs) 2

-- merge sort
msort :: Ord a => [a] -> [a]
msort [] = []
msort xs@(x:[]) = xs
msort xxs = merge (msort xs) (msort ys)
 where (xs, ys) = halve xxs

-- mysum
mysum :: [Int] -> Int
mysum [] = 0
mysum (x:xs) = x + mysum xs

-- mytake
mytake :: Int -> [a] -> [a]
mytake _ [] = []
mytake 0 _ = []
mytake n (x:xs) = x : mytake (n-1) xs

-- mylast
mylast :: [a] -> Maybe a
mylast [] = Nothing
mylast (x:[]) = Just x
mylast (_:xs) = mylast xs

