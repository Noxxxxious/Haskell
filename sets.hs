powerset :: [Int] -> [[Int]]
powerset [] = [[]]
powerset (x:xs) = map (x:) (powerset xs) ++ powerset xs

modifyList :: [[Int]] -> [(Int, Int, [Int])]
modifyList [[]] = [(0,0,[])]
modifyList (x:xs) = (length x, sum x, x) : modifyList xs

commonPart :: [Int] -> [Int] -> [Int]
commonPart _ [] = []
commonPart [] _ = []
commonPart a b = [x | x <- a, x `elem` b]

findPairs :: (Int, Int, [Int]) -> [(Int, Int, [Int])] -> [(Int, Int, [Int])]
findPairs (a,b,c) d = [(e,f,g) | (e,f,g) <- d, e==a, f==b, null (c `commonPart` g)]

filtering :: [(Int, Int, [Int])] -> [(Int, Int, [Int])]
filtering x = [(a,b,c) | (d,e,f) <- x, (a,b,c) <- findPairs (d,e,f) x, (a,b,c) /= (d,e,f)]

removeDuplicates :: [(Int, Int, [Int])] -> [(Int, Int, [Int])]
removeDuplicates x = fun x [] where
    fun [] arg = arg
    fun (x:xs) arg
        |x `elem` arg = fun xs arg
        |otherwise = fun xs (x:arg)

findSets :: [Int] -> [(Int, Int, [Int])]
findSets set = removeDuplicates j where
    j = filtering k 
    k = modifyList (powerset set)


result :: [Int] -> String 
result input
    | null result = "There are no such sets."
    | otherwise = "Such sets have been found: " ++ show result
    where
        result = findSets input