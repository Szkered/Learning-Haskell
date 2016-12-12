twoSum :: Int -> [(Int, Int)] -> (Int, Int)
twoSum _ [] = (-1, -1)
twoSum _ [x] = (-1, -1)
twoSum n (x:xs) = 
    -- let re = filter (\y -> (snd y + snd x) == n) xs
    let re = filter ((==n) . (+snd x) . snd) xs
    in if not (null re) then (fst x, fst (head re)) else twoSum n xs

twoSum' :: Int -> [Int] -> (Int, Int)
twoSum' n l = twoSum n (zip [0..] l)

twoSum'' :: (Integral a) => a -> [a] -> (a, a)
twoSum'' _ [] = (-1, -1)
twoSum'' _ [x] = (-1, -1)
twoSum'' n l = head [(i, j) | (i, a) <- enum l, (j, b) <- enum l, i/=j, a+b==n]
    where enum x = zip [0..] x