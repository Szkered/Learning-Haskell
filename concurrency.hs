-- https://www.schoolofhaskell.com/school/advanced-haskell/beautiful-concurrency/3-software-transactional-memory
import System.IO
import Data.IORef
import Control.Concurrent
import Control.Concurrent.STM

-- how to use IORef
incRef var = do val <- readIORef var
                writeIORef var (val+1)

main' = do var <- newIORef 42
           incRef var
           val <- readIORef var
           hPutStr stdout (show val)
           tid <- forkIO (hPutStr stdout "Hello")
           hPutStr stdout " world\n"
           hPutStr stdout (show tid)


type Account = TVar Int

-- notice that the whole do block in withdraw is in STM Monad
withdraw :: Account -> Int -> STM ()
withdraw acc amount = do bal <- readTVar acc
                         writeTVar acc (bal - amount)

deposite :: Account -> Int -> STM ()
deposite acc amount = withdraw acc (- amount)

transfer :: Account -> Account -> Int -> IO ()
transfer from to amount = atomically (do deposite to amount
                                         withdraw from amount)


-- this won't compile.. because actions are not in the same Monad (context)
-- bad acc = do hPutStr stdout "Withdrawing..."
--              withdraw acc 10

-- function 'atomically' takes STM monad and output IO monad
good acc = do hPutStr stdout "Withdrawing..."
              atomically (withdraw acc 10)

main'' = do acc <- atomically (newTVar 200)
            good acc
            hPutStr stdout "\nDone!\n"
            showAcc "account" acc


-- BLOCKING
-- Ex: a limited withdraw that waits for deposit

-- retry
limitedWithdraw' :: Account -> Int -> STM ()
limitedWithdraw' acc amount = do bal <- readTVar acc
                                 if amount > 0 && amount > bal
                                    then retry
                                    else writeTVar acc (bal - amount)

-- check is just a syntatic of retry. It's a library function
check' :: Bool -> STM ()
check' True  = return ()
check' False = retry

limitedWithdraw :: Account -> Int -> STM ()
limitedWithdraw acc amount = do bal <- readTVar acc
                                check' (amount <= 0 || amount <= bal)
                                writeTVar acc (bal - amount)

-- delay is in milisecond, so 3000000 = 3 secs
delayDeposit acc amount delay = do threadDelay delay
                                   hPutStr stdout "Depositing right now!\n"
                                   atomically (do bal <- readTVar acc
                                                  writeTVar acc (bal + amount))


main2 = do acc <- atomically (newTVar 100)
           forkIO (delayDeposit acc 1 1000000)
           hPutStr stdout "Withdrawing...\n"
           atomically (limitedWithdraw acc 101)
           hPutStr stdout $ "Oh!!! "
           showAcc "account" acc
           -- hPutStr stdout $ "bal: " ++ (show bal)

-- CHOICE
-- orElse
limitedWithdraw2 :: Account -> Account -> Int -> STM ()
limitedWithdraw2 acc1 acc2 amount =
  orElse (limitedWithdraw acc1 amount) (limitedWithdraw acc2 amount)

showAcc name acc = do bal <- atomically (readTVar acc)
                      hPutStr stdout (name ++ ": $")
                      hPutStr stdout (show bal ++ "\n")

main = do acc1 <- atomically (newTVar 100)
          acc2 <- atomically (newTVar 100)
          showAcc "Left pocket" acc1
          showAcc "Right pocket" acc2
          forkIO (delayDeposit acc2 1 3000000)
          forkIO (delayDeposit acc1 1 2000000)
          hPutStr stdout "Withdrawing...\n"
          atomically (limitedWithdraw2 acc1 acc2 101)
          showAcc "Left pocket" acc1
          showAcc "Right pocket" acc2

-- a possible TVar exec model: optimistic execution
-- 1. when (atomically act) is performed, a thread-local transaction log is allocated
-- 2. run (or simulate) act, which does write to TVar but rather the transaction log
-- 3. validate the logs by comparing real TVar value with what's in the log (the first readTVar?)
-- 4. if validation fail, re-initialize the log and re-execute the act
-- a. retry: undo the current transaction, until a writeTVar happends

-- the key idea is that when using (atomically act), act is packaged into a single atomic action
