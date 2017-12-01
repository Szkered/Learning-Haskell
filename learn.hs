import Data.List
import Data.Char
import Data.Function
import qualified Data.Map as Map
import Control.Monad
import System.IO
import Control.Applicative


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
    | even n = n : chain (n `div` 2)
    | odd n  = n : chain (n * 3 + 1)

numLongChains :: (Num a) => a
-- numLongChains = fromIntegral (length (filter ((>15) . length) (map chain [1..100])))
numLongChains = genericLength . (filter ((>15) . length)) . (map chain) $ [1..100]

-- (:) x xs should read as: front append x to list xs
-- flip (:) xs x
-- foldl f acc arg read as:: do f acc a where a:as = arg
reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

sqrtSums :: Int
sqrtSums = (+1) . length . takeWhile (<1000) . scanl1 (+) . map sqrt $ [1..]

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort front) (mergesort rear)
    where (front, rear) = splitAt ((genericLength xs) `div` 2) xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge as [] = as
merge [] bs = bs
merge (a:as) bs = b_small ++ (merge b_big (a:as))
  where b_small = takeWhile (<=a) bs
        b_big   = dropWhile (<=a) bs

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
class Eq' a where
  (.==) :: a -> a -> Bool
  (./=) :: a -> a -> Bool
  x .== y = not (x ./= y)
  x ./= y = not (x .== y)

data TrafficLight = Red | Yellow | Green
instance Eq' TrafficLight where
  Red .== Red = True
  Green .== Green = True
  Yellow .== Yellow = True
  _ .== _ = False

instance (Eq' m) => Eq' (Maybe m) where
  Just x .== Just y = x .== y
  Nothing .== Nothing = True
  _ .== _ = False

class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

instance Functor' [] where
    fmap' = map

-- mathematically functors are homs between cats
-- here Node is functor that maps arbitrary type a to a singleton Tree Node a
instance Functor' Tree where
    fmap' f EmptyTree = EmptyTree
    fmap' f (Node x leftsub rightsub) = Node (f x) (fmap' f leftsub) (fmap' f rightsub)

-- typeclass 102: kind inference
class Tofu t where
    tofu :: j a -> t a j --here t must have the kind * -> (* -> *) -> *

data Frank a b = Frank {frankField :: b a} deriving (Show) --same here with Frank

--hence
instance Tofu Frank where
    tofu x = Frank x


-- I/O
main = main3

main' = do
    return ()
    return "HAHAHA"
    line <- getLine
    return "Blah"
    return 4
    putStrLn line

main'' = do
    c <- getChar
    if c /= ' '
        then do
            putChar c
            main''
        else return ()

main1 = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main1

main2 = do
    rs <- sequence [getLine, getLine, getLine]
    print rs

main3 = do
    colors <- forM [1..4] (\a -> do
        putStrLn $ "Associate color with the number " ++ show a ++ ":"
        color <- getLine
        return color)
    putStrLn "colors choosen:"
    mapM putStrLn colors

main4 = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\l -> length l < 25) . lines

main5 = do
    handle <- openFile "TwoSum.hs" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

main6 = do
    withFile "TwoSum.hs" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)


main7 = do
    line <- fmap reverse getLine
    putStrLn $ "You said " ++ line ++ " backwards!"

-- functionally solving problems
solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
    where foldingFunction (x:y:ys) "*" = (y * x):ys
          foldingFunction (x:y:ys) "+" = (y + x):ys
          foldingFunction (x:y:ys) "-" = (y - x):ys
          foldingFunction (x:y:ys) "/" = (y / x):ys
          foldingFunction (x:y:ys) "^" = (y ** x):ys
          foldingFunction (x:ys) "ln"  = log x:ys
          foldingFunction xs "sum"     = [sum xs]
          foldingFunction xs numStr    =  read numStr:xs

-- Functors and Monoids
--   A functor is a type constructor that takes one type, so
-- effectively it is a function that maps between types
-- for a type of kind * -> * to be a functor, it must has a
-- fmap, which is a function that maps a function in domain type
-- to a function to codomain type, i.e. it has the type (a -> b) -> f a -> f b)
--   A functor is infact a map between two categories (types in haskell),
-- where f is the map for objects, fmap is the map for morphisms in categories.
-- therefore, fmap must preserve the identity function and function composition.


-- this functor takes a type r and returns (r -> a) which is a function
-- we see that its fmap is just functional composition
instance Functor' ((->) r) where
    fmap' = (.)

-- an invalid Functor, as it doesn't preserve function composition
data CMaybe a = CNothing | CJust Int a deriving (Show)
instance Functor' CMaybe where
    fmap' f CNothing = CNothing
    fmap' f (CJust counter x) = CJust (counter+1) (f x)
