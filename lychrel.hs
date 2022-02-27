d :: Int -> Int -> Int 
d x i= (x `mod` 10^(i+1) - x `mod` 10^i) `div` (10^i)

f :: Int -> Int
f n = n + sum ([d n i * 10^(k-i-1) | i <- [0..k-1]]) where
    k = floor (logBase 10 nn) + 1 where
        nn = fromIntegral n

isPalindrome :: Int -> Bool
isPalindrome n =
    s == reverse s where
        s = show n

procLychrel :: Int -> Int -> Int -> Int 
procLychrel n prevx i
    | i > n = -1 
    | isPalindrome prevx = prevx
    | x == 2 * prevx = x
    | otherwise = procLychrel n x (i+1)
    where 
        x = f prevx

isLychrel :: Int -> Int -> String
isLychrel n x
    | result == -1 = "No counterexample has been found after given number of transformations."
    | otherwise = "Counterexample: " ++ show result
    where
        result = procLychrel n x 0