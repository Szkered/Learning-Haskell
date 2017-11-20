import Control.Monad
import Control.Applicative

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
-- 3:
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
data R a = R (Resource -> (Resource, Either a (R a))) -- recursive infinite data structure

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
  return v     = R (\r -> (r, (Left v)))

-- perform resource usage
step v = c where
           c = R (\r -> if r /= 0
                       then (r-1, Left v)
                       else (r, Right c))

inc i = do iValue <- i
           step (iValue + 1)

lift1 f = \ra1 -> do a1 <- ra1
                     step (f a1)

inc' i = lift1 (1+) i

lift2 f = \ra1 ra2 -> do a1 <- ra1
                         a2 <- ra2
                         step (f a1 a2)
