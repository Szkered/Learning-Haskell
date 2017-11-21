module TreeADT (Tree, leaf, branch, cell, left, right, isLeaf) where

data Tree a        = Leaf a | Branch (Tree a) (Tree a)
leaf               = Leaf
branch             = Branch
cell (Leaf a)      = a
left (Branch l r)  = l
right (Branch l r) = r
isLeaf (Leaf _)    = True
isLeaf _           = False

-- TODO understand why use ADT
-- the point of doing this is that now we have abstract operations
-- that are TYPED. Effectively I can change the representation of
-- type Tree later on to, for example, to accommendate node value,
-- which could be something like
-- data Tree' a = EmptyTree | Node a (Tree' a) (Tree' a) deriving (Show, Read, Eq)
-- or
-- data Tree'' a = Leaf' a | Branch' a (Tree'' a) (Tree'' a) deriving (Show, Read, Eq)
