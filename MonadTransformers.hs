{-# LANGUAGE InstanceSigs #-}
import Data.Char
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class

main = askPassphrase

getPassphrase = do s <- getLine
                   if isValid s
                      then return $ Just s
                      else return Nothing

isValid s = length s>= 8
            && any isAlpha s
            && any isNumber s
            && any isPunctuation s

askPassphrase = do putStrLn "Insert your new passphrase:"
                   mVal <- getPassphrase
                   case mVal of
                     Just val -> do putStrLn "Success"
                                    putStrLn "Exit"
                     Nothing  -> putStrLn "invalid passphrase"

-- Monad Transformers come to rescue!
-- MaybeT :: (* -> *) -> * -> *
-- m :: * -> *    an arbitrary Monad
-- a :: *         an arbitrary type of kind *
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
-- runMaybeT accessor remove the MaybeT wrapper and
-- return a value of form m (Maybe a)
-- the term constructor Maybe T takes an arbitrary Monad m, which wraps a Maybe value

instance Monad m => Monad (MaybeT m) where
         return =  MaybeT . return . Just
         x >>= f = MaybeT $ do mVal <- runMaybeT x -- the whole do block is in m Monad!
                               case mVal of
                                 Nothing  -> return Nothing
                                 Just val -> runMaybeT $ f val

-- Since Maybe is a instance of Alternative and MonadPlus, let's implement these as well
instance Monad m => Alternative (MaybeT m) where
         empty   = MaybeT $ return Nothing
         x <|> y = MaybeT $ do mVal <- runMaybeT x
                               case mVal of
                                 Nothing -> runMaybeT y
                                 Just _  -> return mVal

instance Monad m => MonadPlus (MaybeT m) where
         mzero = empty
         mplus = (<|>)

instance MonadTrans MaybeT where
         lift = MaybeT . (liftM Just)

instance (Functor m, Monad m) => Applicative (MaybeT m) where
         pure = return
         (<*>) = ap

instance Functor m => Functor (MaybeT m) where
         fmap = mapMaybeT . fmap . fmap

mapMaybeT :: (m (Maybe a) -> n (Maybe b)) -> MaybeT m a -> MaybeT n b
mapMaybeT f = MaybeT . f . runMaybeT


getPassphrase' :: MaybeT IO String
getPassphrase' = do s <- lift getLine
                    guard (isValid s)
                    return s

askPassphrase' :: MaybeT IO ()
askPassphrase' = do lift $ putStrLn "insert your new passphrase:"
                    value <- getPassphrase'
                    lift $ putStrLn "Storing passphrase..."


