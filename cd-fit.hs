import Text.ParserCombinators.Parsec
import Data.List
import Data.Array
-- import Test.QuickCheck
import Control.Monad (liftM2, replicateM)


parseInput =
  do dirs <- many dirAndSize
     eof::Parser ()
     return dirs

data Dir = Dir { dir_size::Int, dir_name::String } deriving (Show, Eq)
data DirPack = DirPack { pack_size::Int, dirs::[Dir] } deriving (Show, Eq)

dirAndSize =
  do size <- many1 digit
     spaces
     dir_name <- anyChar `manyTill` newline
     return (Dir (read size) dir_name)

main :: IO ()
main = do line <- getLine
          let maybeInt = readMaybe line :: Maybe Int
          case maybeInt of
            Just n  -> do
              print $ "media size: " ++ show n
              input <- getContents
              putStrLn ("debug: input\n" ++ input)
              let dirs = case parse parseInput "stdin" input of
                              Left err -> error $ "Input:\n" ++ show input ++
                                                  "\nError:\n" ++ show err
                              Right result -> result
                  total_size = foldr (\d -> (+) (dir_size d)) 0 dirs
              putStrLn ("total size: " ++ show total_size)
              putStrLn "\nGreedy solution"
              print (greedy_pack dirs n)
              putStrLn "\nDynamic programming solution"
              print (dynamic_pack dirs n)
            Nothing -> putStrLn "error"

readMaybe s = case reads s of
                [(val, "")] -> Just val
                _           -> Nothing

-- media_size = 700*1024*1024      -- 700MB CD volume

-- simple greedy solution
greedy_pack in_dirs media_size = foldl (maybe_add_dir media_size) (DirPack 0 []) $ sortBy cmpSize in_dirs
  where cmpSize d1 d2 = compare (dir_size d1) (dir_size d2)
        maybe_add_dir m p d =
          let new_size = pack_size p + dir_size d
              new_dirs = d:(dirs p)
              in if new_size > m then p else DirPack new_size new_dirs


-- -- QuickCheck
-- instance Arbitrary Dir where
--          -- coarbitrary = undefined
--          arbitrary = liftM2 Dir gen_size gen_name
--            where gen_size = do s <- choose (10,1400)
--                                return (s*1024*1024)
--                  gen_name = do n <- choose (1,300)
--                                replicateM n (elements "fubar/")
-- prop_greedy_pack_is_fixpoint ds =
--                              let pack = greedy_pack ds
--                              in pack_size pack == pack_size (greedy_pack (dirs pack))

-- Dynamic programming approach
precomputeDisksFor dirs =
  let precomp = map bestDisk [0..]
      bestDisk 0     = DirPack 0 [] -- best packed disk of size 0 is empty
      bestDisk limit =              -- recursive definition
        case [ DirPack (dir_size d + s) (d:ds)
               | d <- filter ( (inRange (1,limit)) . dir_size ) dirs  -- filter dir with size less than limit
                 , dir_size d > 0                                     -- safety check
                 , let (DirPack s ds) = precomp!!(limit - dir_size d) -- get the best packed disk recursively
                 , d `notElem` ds
             ] of
             [] -> DirPack 0 []               -- negative case
             packs -> maximumBy cmpSize packs -- get the optimal pack
      cmpSize a b = compare (pack_size a) (pack_size b)
  in precomp

dynamic_pack dirs media_size = (precomputeDisksFor dirs)!!media_size
