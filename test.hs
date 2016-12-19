import Data.List
import Data.Char
import Data.Function
import qualified Data.Map as Map

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
numLongChains = genericLength . (filter ((>15) . length)) . (map chain) $ [1..100]

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

sqrtSums :: Int 
sqrtSums = (+1) . length . takeWhile (<1000) . scanl1 (+) . map sqrt $ [1..]

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort front) (mergesort rear)
    where (front, rear) = splitAt ((genericLength xs) `div` 2) xs
          merge xs [] = xs
          merge [] ys = ys
          merge (x:xs) ys = (takeWhile (<=x) ys) ++ (merge (dropWhile (<=x) ys) (x:xs))

-- alternative merge
merge' :: (Ord a) => [a] -> [a] -> [a]
merge' xs [] = xs
merge' [] ys = ys
merge' (x:xs) (y:ys)
  | x<=y      = x:(merge' xs (y:ys))
  | otherwise = y:(merge' ys (x:xs))

-- alternative split
splitList :: [a] -> ([a], [a])
splitList [] = ([], [])
splitList [x] = ([x], [])
splitList (x:y:zs) = let (xs, ys) = splitList zs in (x:xs, y:ys)

splitList' :: [a] -> ([a], [a])
splitList' zs = go zs [] []
  where go [] xs ys = (xs, ys)
        go [x] xs ys = (x:xs, ys)
        go (x:y:zs) xs ys = go zs (x:xs) (y:ys)

splitList'' :: [a] -> ([a], [a])
splitList'' xs = (go odd, go even)
  where go f = map snd . filter (f.fst) $ indexed
        indexed = zip [0..] xs
        
mergesort' :: (Ord a) => [a] -> [a]
mergesort' [] = []
mergesort' [x] = [x]
mergesort' xs = merge' (mergesort' front) (mergesort rear)
    where (front, rear) = splitList xs
    
words' :: String -> [String]
words' = filter (not . any isSpace) . groupBy ((==) `on` isSpace)

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs)
    | key == k  = Just v
    | otherwise = findKey key xs

    
data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = 
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ "is taken!"

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

-- binary tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a  = Node a (treeInsert x left) right
  | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a  = treeElem x left
  | x > a  = treeElem x right

-- typeclass
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x == y = not (x /= y)
  x /= y = not (x == y)

data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

instance Eq (Maybe m) where
  Just x == Just y = x == y
  Nothing == Nothing = True
  _ == _ = False

class Functor' f where
    fmap' :: (a -> b) -> f a -> f b
    
instance Functor' [] where
    fmap' = map
    
instance Functor' Tree where
    fmap' f EmptyTree = EmptyTree
    fmap' f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)