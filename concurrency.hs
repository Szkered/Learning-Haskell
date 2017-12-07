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


-- this won't compile..
-- bad acc = do hPutStr stdout "Withdrawing..."
--              withdraw acc 10

-- atomically takes STM monad and output IO monad
good acc = do hPutStr stdout "Withdrawing..."
              atomically (withdraw acc 10)

main'' = do acc <- atomically (newTVar 200)
            good acc
            hPutStr stdout "\nDone!\n"

-- retry and threadDelay
limitedWithdraw' :: Account -> Int -> STM ()
limitedWithdraw' acc amount = do bal <- readTVar acc
                                 if amount > 0 && amount > bal
                                    then retry
                                    else writeTVar acc (bal - amount)

limitedWithdraw :: Account -> Int -> STM ()
limitedWithdraw acc amount = do bal <- readTVar acc
                                check (amount <= 0 || amount <= bal)
                                writeTVar acc (bal - amount)

delayDeposit acc amount = do threadDelay 3000000
                             hPutStr stdout "Depositing right now!\n"
                             atomically (do bal <- readTVar acc
                                            writeTVar acc (bal + amount))


main2 = do acc <- atomically (newTVar 100)
           forkIO (delayDeposit acc 1)
           hPutStr stdout "Withdrawing...\n"
           atomically (limitedWithdraw' acc 101)
           hPutStr stdout $ "Oh!!! "
           showAcc "account" acc
           -- hPutStr stdout $ "bal: " ++ (show bal)


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
          forkIO (delayDeposit acc2 1)
          hPutStr stdout "Withdrawing...\n"
          atomically (limitedWithdraw2 acc1 acc2 101)
          showAcc "Left pocket" acc1
          showAcc "Right pocket" acc2
