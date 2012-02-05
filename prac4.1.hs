halve :: [a] -> ([a], [a])
halve xs = splitAt (div (length xs) 2) xs

-- argument pattern match 
mytake :: Int -> [a] -> [a]
mytake _ [] = []
mytake 0 _ = []
mytake n (x:xs) = x:mytake (n-1) xs

