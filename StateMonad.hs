import           Control.Applicative
import           Control.Monad

main :: IO()
main = do
  print "what's your name?"
  name <- getLine
  print ("hello " ++ name ++ "!")


---------------------------------------------------------------------------
--                                 MONAD                                 --
-- First of all, monad is a type class
-- There's two basic operators:
   -- >>= (bind) : m a -> (a -> m b) -> b
   -- return     : a -> m a
-- >>=: sequentially compose two actions, passing any value produced by
-- the first as an argument to the second.
-- syntatic sugar: do p <- e1; e2 = e1 >>= \p -> e2
-- Return: inject a value into the mondaic type
-- Laws
-- return a >>= k           = k a
-- m >>= return             = m
-- xs >>= return . f        = fmap f xs
-- m >>= (\x -> k x >>= h)  = (m >>= k) >>= h
-- first 2 laws: bind and return are inverses
-- 3. f :: a -> b, return :: b -> m b
-- 4. association rule
---------------------------------------------------------------------------

-- The State Monad is built around the state type S
-- This is one of the possible (and simplest) definition
data S = State1 | State2 deriving (Enum)

-- State Monad wrap a value of type a around a
-- state transition function S -> (a,S)
-- where S is the input state, and a is the value
-- to be encased
data SM a = SM (S -> (a,S))

instance Functor SM where
  fmap = liftM -- promote a function to a monad
instance Applicative SM where
  pure = return
  (<*>) = ap

-- bind: SM (S -> (c1,S))
instance Monad SM where
  SM c1 >>= fc2 = SM (\s0 -> let (r,s1) = c1 s0     -- c1, c2 are unary state transition functions
                                 SM c2  = fc2 r in
                               c2 s1)
  return k = SM (\s -> (k,s))

readSM :: SM S
readSM = SM (\s -> (s,s))

updateSM :: (S -> S) -> SM () --alters the state
updateSM f = SM (\s -> ((), f s))

runSM :: S -> SM a -> (a,S)
runSM s0 (SM c) = c s0


-- now we start to look at a more complicated example
-- we will define a Monad that describes resourceful computation
type Resource = Integer

-- as before, the monad wrap a state transition functions
-- but this time the new state is wrapped in Either
data R a = R (Resource -> (Resource, Either a (R a)))
     -- recursive infinite data structure

instance Functor R where
  fmap = liftM
instance Applicative R where
  pure = return
  (<*>) = ap

instance Monad R where
  R c1 >>= fc2 = R (\r -> case c1 r of
                       (r', Left v) -> let R c2 = fc2 v in
                                         c2 r'
                       (r', Right pc1) -> (r', Right (pc1 >>= fc2)))
  return v     = R (\r -> (r, (Left v))) -- just like the constant function

-- perform resource consumption with identity function
step v = c where
           c = R (\r -> if r /= 0
                       then (r-1, Left v)
                       else (r, Right c))

-- direct implementation of increment function
inc i = do iValue <- i
           step (iValue + 1)

lift1 f = \ra1 -> do a1 <- ra1
                     step (f a1)

-- increment function with lift
inc' i = lift1 (+1) i

lift2 f = \ra1 ra2 -> do a1 <- ra1
                         a2 <- ra2
                         step (f a1 a2)
-- equal to ra1 >>= (\a1 -> ra2 >>= \a2 -> step (f a1 a2))

-- Notice that comparison function wrap boolean result in R monad
-- TODO infact it's impossible to unwrap can get a plain boolean ?
  -- I think this is mainly because to do that one need to get the parameters
  -- out from the resource consumption function, which leads to a
  -- comparison between function, which is not computable
-- Therefore we cannot lift the Eq class.
(==*) :: Ord a => R a -> R a -> R Bool
(==*) = lift2 (==)

-- Alternately, just make a instance declaration of Num class
instance Num a => Num (R a) where
  (+)         = lift2 (+)
  (-)         = lift2 (-)
  negate      = lift1 negate
  (*)         = lift2 (*)
  abs         = lift1 abs
  fromInteger = return . fromInteger
  signum      = lift1 id -- just a stab, not sure what's the best way to do this..

-- now we can define the increment function by giving corresponding type signature
inc'' :: R Integer -> R Integer
inc'' x = x + 1

-- now conditional
ifR :: R Bool -> R a -> R a -> R a
ifR tst thn els = do t <- tst
                     if t
                        then thn
                        else els

-- good ol' factorial function with resourceful computation
fact x = ifR (x ==* 0) 1 (x * fact (x - 1))

-- This is how we run resourceful computation:
-- first compose the program p, then feed in initial resource s
run s (R p) = case (p s) of
                (_, Left v) -> Just v
                _           -> Nothing

-- TODO why?
-- run 16 (fact 3) = Just 6
-- run 9 (fact 2) = Just 2
-- run 4 (fact 1) = Just 1

-- resourceful parallel computation!
c1 ||| c2 = oneStep c1 (\c1' -> c2 ||| c1')               -- swap suspended c1' with c2 as current computation
  where
        oneStep (R c1) f =
          R (\r -> case c1 1 of                           -- run c1 with 1 resource
                     (r', Left v) -> (r + r' - 1, Left v) -- TODO why?
                     (r', Right c1') ->                   -- if suspended
                       let R next = f c1' in              -- compose monad with f and extract the computation next
                       next (r + r' - 1))                 -- run next with r+r'-1 resources

-- if we do
-- run res (fact a ||| fact b)
-- the program will return the one that finishes first
