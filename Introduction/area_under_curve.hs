import Text.Printf (printf)

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = [area, volume]
    where eval p = sum . map (\(ai, bi) -> ai * p ^^ bi) $ zip af b
          area = foldl (\acc p -> acc + 0.001 * eval p) 0 range
          volume = foldl (\acc p -> acc + 0.001 * pi * eval p ^ 2) 0 range
          range = map (\x -> (fromIntegral x :: Double ) / 1000) [l*1000..r*1000]
          af = map fromIntegral a :: [Double]


--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
