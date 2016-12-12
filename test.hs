factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

head' :: [a] -> a
head' [] = error "can't call head on empty list!"
head' (x:_) = x

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "hey you!"
    | otherwise   = "whatever"

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : (zip' xs ys)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
-- quicksort (x:xs) =
--     let smallerSorted = quicksort (filter (<=x) xs)
--         largerSorted  = quicksort (filter (>x) xs)
--     in smallerSorted ++ [x] ++ largerSorted
quicksort (x:xs) = quicksort (filter (<=x) xs) ++ [x] ++ quicksort (filter (>x) xs)
    
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- Collatz sequence
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n  = n:chain (n*3 + 1)

numLongChains :: (Num a) => a
-- numLongChains = fromIntegral (length (filter ((>15) . length) (map chain [1..100])))
numLongChains = fromIntegral . length . (filter ((>15) . length)) . (map chain) $ [1..100]

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

sqrtSums :: Int 
sqrtSums = (+1) . length . takeWhile (<1000) . scanl1 (+) . map sqrt $ [1..]