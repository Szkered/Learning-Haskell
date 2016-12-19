import Control.Applicative
import Control.Monad
import System.IO


main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    forM_ [1..n] $ \a0  -> do
        x_temp <- getLine
        let x = read x_temp :: Double


getMultipleLines :: Int -> IO [String]

getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret          

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

exp' :: Double -> Double
exp' = (+1) . sum . map (\(i, x) -> (x ^ i) / (fromIntegral . factorial $ i)) . zip [1..9] . replicate 10