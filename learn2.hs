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
main = do 
    putStrLn . show. getZipList $ liftA2 max (ZipList [1,4,2,5,6]) (ZipList [6,4,7,3,6,7,8])

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs


-- newtype
newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)