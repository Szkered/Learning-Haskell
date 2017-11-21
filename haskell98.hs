module Haskell98 where          -- Haskell's module design is very simple: completely flat namespace!
-- but what if we have clashing names in different module?
-- simple! use qualified name: Module.Entity (i.e. non-flat name)

import Control.Exception
import System.IO.Error
import Data.Ix
import Data.Array

-- main :: IO()
-- main = do
--   print "what's your name?"
--   name <- getLine
--   print ("hello " ++ name ++ "!")

---- POLYMORPHIC TYPES ----------------------------------------------------
length' :: [a] -> Integer
length' []     = 0
length' (x:xs) = 1 + length' xs

---- USER-DEFINED TYPES --------------------------------------
-- syntax for data: type constructor = data constructor
data Bool' = False' | True'        -- nullary data constructors
data Point a = Pt a a           -- unary type contructors

-- Type Synonyms
type Name    = String
-- type Address = None | Addr String => this would not work
data Address = None | Addr String

fringe :: Tree a -> [a]
fringe (Leaf x)            = [x]
fringe (Branch left right) = fringe left ++ fringe right

-- bottom => non-terminating expression
bot = bot

-- Infinite Data Structure built by recursive pattern matching
ones        = 1 : ones
fib         = 1 : 1 : [ a+b | (a,b) <- zip fib (tail fib)]

-- THREE kinds of irrefutable patterns
fib'@(x:xs) = 1 : 1 : [ a+b | (a,b) <- zip fib' xs] -- 1. AS-PATTERN
head' (x:_) = x                                     -- 2. WILDCARD
-- 3. LAZY PATTERNS

-- pattern matchings are left to right, top to bottom
take1 0 _     = []
take1 _ []    = []
take1 n (x:xs) = x : take1 (n-1) xs

take1' _ []    = []
take1' 0 _     = []
take1' n (x:xs) = x : take1' (n-1) xs

---- LAZY PATTERN - client server simulation ----------------
init' = Leaf 1
reqs = client init' resps
resps = server reqs

client init' ~(resp:resps) = init' : client (next resp) resps
next resp = resp

server (req:reqs) = process req : server reqs
process req = Branch req req

-- The following line wouldn't work,
-- because the follwing pattern try to match resp, which doesn't exist yet
-- client init' (resp:resps) = init' : client (next resp) resps

-- client init' resps = init' : client (next (head resps)) (tail resps) -- non elegant way


---- LEXICAL SCOPING AND NESTED FORMS ---------------------------
-- LET: bindings are mutually recursive and lazy

-- let y   = a*b
--     f x = (x+y)/y
-- in f c + f d

-- WHERE: scope over multiple expression

---- TYPE CLASS -------------------------------------------------
-- Recursive Type Class
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)

-- functor is a type class where the
-- [((i,1), 1) | i <= [2..n]] ++
-- fmap applies normal function over ktype constructor
instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Branch t1 t2) = Branch (fmap f t1) (fmap f t2)

instance (Eq a) => Eq (Tree a) where
  Leaf a == Leaf b                 = a == b
  (Branch l1 r1) == (Branch l2 r2) = (l1==l2) && (r1==r2)
  _ == _                           = False

---- NEWTYPE: Make NEW type out of existing type --------------------------
newtype Natural = MakeNatural Integer -- unlike type, it's not just a synonym

toNatural x | x < 0     = error "Cant't create negative naturals!"
            | otherwise = MakeNatural x

fromNatural (MakeNatural i) = i

instance Num Natural where
  fromInteger = toNatural
  x + y       = toNatural (fromNatural x + fromNatural y)
  x - y       = let r = fromNatural x - fromNatural y in
                  if r < 0 then error "Unnatural subtraction"
                  else toNatural r
  x * y       = toNatural (fromNatural x * fromNatural y)
  abs x       = x
  signum x    = 1

  -- this can be implemented by using data
  -- but that will incur the overhead of

---- Strict Data constructor ----------------------------------
data Complex a = !a :+ !a -- data fields marked by ! are strict
-- colon means infix operator

---- IO ---------------------------------------
todoList = [putChar 'a',
            do putChar 'b'
               putChar 'c',
            do c <- getChar
               putChar c]

sequence1 []     = return ()
sequence1 (a:as) = do a
                      sequence1 as

sequence2 :: [IO ()] -> IO ()
sequence2 = foldr (>>) (return ()) -- because x; y equals x >> y

putStr' s = sequence2 (map putChar s)

-- ERROR HANDLING -------------------------------------------------
getChar' = getChar `catch` eofHandler where
  eofHandler e = if isEOFError e then return '\n' else ioError e

getLine' :: IO String
getLine' = let eofHandler e = if isEOFError e then return "\n" else ioError e
           in getLine'' `catch` eofHandler
  where getLine'' = do c <- getChar'
                       if c == '\n'
                          then return ""
                          else do l <- getLine'
                                  return (c:l)

-- SHOW & READ ----------------
showsTree (Leaf x) = shows x
showsTree (Branch l r) = ('<':) . showsTree l . ('|':) . showsTree r . ('>':)

-- method that parse a tree and returns [(Tree, unparsed remains)]
readsTree ('<':s) = [(Branch l r, u) | (l, '|':t) <- readsTree s,
                                       (r, '>':u) <- readsTree t]
readsTree s       = [(Leaf x, t) | (x, t) <- reads s :: [(Int, String)]]

-- however this parse cannot handle white space;
-- using lex will solve the problem as it extract the first lexeme
readsTree' s = [(Branch l r, x) | ("<", t) <- lex s,
                                  (l, u)   <- readsTree' t,
                                  ("|", v) <- lex u,
                                  (r, w)   <- readsTree' v,
                                  (">", x) <- lex w]
               ++
               [(Leaf x, t)     | (x, t) <- reads s::[(Int, String)]]

-- NUMBERS -----------------------------------------------------
{-
Eq => Num, but Ord /=> Num
Num does not provide division
Num => Integral: unbounded int, or bignums
Num => Int     : bounded int, with a range to at least 29-bits signed binary
Num => Franctional

Primitives: Int, Integer, Float, Double

numeral literals (like 8) are actually fromInteger 8::Integer, which has the
type (Num a) => a
-}

rms x y = sqrt ((x^2 + y^2) * 0.5)
-- (^) has the type (Num a, Integral b) => a -> b -> a
-- Now from the above definition we only knows that 2 is in the Integral class
-- a simple way to fix it would be
rms' x y = sqrt ((x^(2::Integer) + y^(2::Integer)) * 0.5)
-- This is tiresome. We can solve this better with ~default declaration~, like this:
-- defalut (Int, Float)
-- the 'default default' is (Integer, Double)

-- ARRAYS ----------------------------------------------------------
mkArray f bnds = array bnds [(i, f i) | i <- range bnds]

fibs n = a where
         a = array (0,n) ([(0, 1), (1, 1)] ++
                          [(i, a!(i-2) + a!(i-1)) | i <- [2..n]])

wavefront n = a where
              a = array ((1,1), (n,n))
                    ([((1,j), 1) | j <- [1..n]] ++
                     [((i,1), 1) | i <- [2..n]] ++
                     [((i,j), a!(i,j-1) + a!(i-1,j-1) + a!(i-1,j))
                     | i <- [2..n], j <- [2..n]])
-- histogram implemented with accumulating function
hist bnds is = accumArray (+) 0 bnds [(i, 1) | i <- is, inRange bnds i]

-- create histogram over measurements on the interval [a,b) in decades (10 bins)
decades a b = hist (0,9) . map decade
              where decade x = floor ((x - a) * s)
                    s        = 10 / (b - a)

-- Array update operator a // [(i,v)]
swapRows i i' a = a // [assoc | j <- [jLo..jHi],
                                assoc <- [((i, j), a!(i',j)),
                                          ((i',j), a!(i, j))] ]
                                          where ((iLo, jLo), (iHi,jHi)) = bounds a
-- what? there's no temp array to hold the swapped value? of course!
-- data are immutable, so we can do this loop fusion easilly

-- look at the type of matMult, it's as generic as it can get
matMult x y = array resultBounds
                    [((i,j), sum [x!(i,k) * y!(k,j) | k <- range (lj,uj)])
                                  | i <- range (li,ui),
                                    j <- range (lj',uj')]
        where ((li,lj),(ui,uj))     = bounds x
              ((li',lj'),(ui',uj')) = bounds y
              resultBounds
                | (lj,uj) == (li',ui') = ((li,lj'),(ui,uj'))
                | otherwise            = error "matMult: incompatible bounds"
