import Control.Applicative

-- Applicative Functor

-- class (Functor' f) => Applicative' f where
--     pure :: a -> f a
    -- <*> :: f (a -> b) -> f a -> f b

-- Applicative functors can fmap a function directly via method 'pure', and
-- apply it later to data of codomain type via operator <*>. For example you
-- can do this in Maybe type:
-- pure (+) <*> Just 3 <*> Just 7
-- In summary, pure f <*> x equals fmap f x
-- so pure f <*> x <*> y .. is just fmap f x <*> y ...
-- Here we introduce the infix version of fmap, <$>, which is good for style:
-- f <$> x <*> y <*> z ..
-- compare it with normal function application:
-- f x y z ..
-- In the context of function, we have
-- instance Applicative ((->) r) where
--     pure x = (\_ -> x)
--     f <*> g = \x -> f x (g x)
-- pure (+) <*> (+3) <*> (*5)

-- instance Applicative [] where
--     pure x = [x]
--     fs <*> xs = [f x | f <- fs, x <- xs]

-- Now here's a power example for applicative IO
-- instance Applicative' IO where
--     pure = return
    -- a <*> b = do
    --     f <- a
    --     x <- b
    --     return (f x)
-- we can prompt twolines and get the concat result back by
main1 = do
    a <- (++) <$> getLine <*> getLine
    putStrLn $ "two lines concatenated is: " ++ a


-- Yet another powerful example: ZipList. ZipWith function can only
-- zip two list together with a function that takes two params. What
-- if we want to zip arbitrary number of lists together?
-- well we can actually do:

main2 = do
    putStrLn . show . getZipList $ max <$> ZipList [1,4,2,5,6] <*> ZipList [6,4,7,3,6,7,8]

-- of course, you can use the more general version of fmap, liftA2 (acutally liftAn)
-- to achieve the same effect, albeit the style puts more strain to the eyes
main3 = do
    putStrLn . show. getZipList $ liftA2 max (ZipList [1,4,2,5,6]) (ZipList [6,4,7,3,6,7,8])

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs


-- newtype
newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

-- Monoid is just a group without guranteed inverses
-- class Monoid m where
--     mempty :: m                     -- identity
--     mappend :: m -> m -> m          -- binary associative operation
--     mconcat :: [m] -> m             -- reduce a list using monoid operation
--     mconcat = foldr mappend mempty


-- Monads!!!
-- they are just beefed up applicatives?

-- class Monad m where
--     return :: a -> m a

--     (>>=) :: m a -> (a -> m b) -> m b

--     (>>) :: m a -> m b -> m b
--     x >> y = x >>= \_ -> y

--     fail :: String -> m a
--     fail msg = error msg

-- (>>=) is the binding function, which feeds a monadic value into
-- a function that transform normal value to a monadic value
-- (>>) is the monadic application without binding: look at the signature
-- some obvious law:
-- return x >>= f EQUALS f x
-- m >>= return EQUALS m
-- (m >>= f) >>= g EQUALS m >>= (\x -> f x >>= g)
--

-- this is the bind method (>>=) for Maybe monad
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f  = Nothing
applyMaybe (Just x) f = f x

-- Monads can be used in a function that could fail. Here's an example.
type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left,right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left,right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

-- note that we cannot do
-- landLeft 1 . landRight 3 $ (0,0)
-- because the return type is a monad and our function accept only normal Birds!
-- instead, we need to do
-- return (0,0) >>= landRight 3 >>= landLeft 1
-- Now compare the monadic implementation and the following one
routine :: Maybe Pole
routine = case landLeft 1 (0,0) of
    Nothing -> Nothing
    Just pole1 -> case landRight 4 pole1 of
        Nothing -> Nothing
        Just pole2 -> case landLeft 2 pole2 of
            Nothing -> Nothing
            Just pole3 -> landLeft 1 pole3

-- in fact, do is a syntatic suger for monad application
foo' :: Maybe String
foo' = Just 3 >>= (\x ->
         Just "!" >>= (\y ->
           Just (show x ++ y)))

-- which can be written as
foo :: Maybe String
foo = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)

-- knowing this, we can actually do
routine' :: Maybe Pole
routine' = do
    start <- return (0,0)
    first <- landLeft 2 start
    second <- landRight 2 first
    landLeft 1 second


-- instance Monad [] where
--     return x = [x]
--     xs >>= f = concat (map f xs)
--     fail _ = []

-- an example:
-- [1,2] >>= \n -> ['a', 'b'] >>= \ch -> return (n,ch)
-- using do notation this would become:
listTuples :: [(Int,Char)]
listTuples = do
    n <- [1,2]
    ch <- ['a','b']
    return (n,ch)

-- in fact, list comprehension is again a syntatic sugar for do notation,
-- which is also a syntatic sugar for monad application (list in particular).
-- We can do the same thing by
listTuples' = [(n,ch) | n <- [1,2], ch <- ['a','b']]
-- but how do we do this in monad application??
list = [x | x <- [1,2], '7' `elem` show x]

-- we are gonna need some extra stuff. First lets make a class for monoid monad
class Monad m => MonoidMonad m where
    mzero :: m a
    mplus :: m a -> m a -> m a

instance MonoidMonad [] where
    mzero = []
    mplus = (++)

guard :: (MonoidMonad m) => Bool -> m ()
guard True = return ()
guard False = mzero

-- now we can finally produce the list
list' = [1,2] >>= (\x -> guard ('7' `elem` show x) >> return x)
-- or in do notation
list'' = do
    x <- [1,2]
    guard ('7' `elem` show x)
    return x

-- Monadically solving the knight problem!
type KnightPos = (Int, Int)

moveKnight :: [KnightPos] -> [[KnightPos]]
moveKnight ps = filter onBoard
    [(c+2,r-1):ps,(c+2,r+1):ps,(c-2,r-1):ps,(c-2,r+1):ps
    ,(c+1,r-2):ps,(c+1,r+2):ps,(c-1,r-2):ps,(c-1,r+2):ps
    ]
    where onBoard ((c,r):rs) = c `elem` [1..8] && r `elem` [1..8]
          (c,r) = head ps

in3 :: KnightPos -> [[KnightPos]]
in3 start = return [start] >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> [[KnightPos]]
canReachIn3 start end = filter ((end==) . head) . in3 $ start
