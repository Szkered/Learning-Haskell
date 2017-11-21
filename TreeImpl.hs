module TreeImpl where
import qualified TreeADT ( Tree, leaf )

data Tree a = Leaf a | Branch a (Tree a) (Tree a) deriving (Show, Read, Eq)
