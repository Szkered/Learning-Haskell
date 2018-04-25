import Control.Concurrent.STM
import Control.Concurrent
import System.Random
import GHC.Conc (numCapabilities)

type Cap = Int
type RemainingCap = Int
data Gate  = MkGate Cap (TVar RemainingCap)

-- initially Remains is 0 so no one can enter the gate
newGate n = do tv <- newTVar 0
               return (MkGate n tv)

-- helper will try to pass the gate by decreasing RemainingCap
-- it will wait until santa does operateGate, which sets RemainingCap to Cap
passGate (MkGate n tv) = atomically (do n_left <- readTVar tv
                                        check (n_left > 0)
                                        writeTVar tv (n_left-1))

-- santa can open the gate by setting Remains back to Cap
-- also, we need the check action as one cannot open gate again unless Remains is 0
operateGate (MkGate n tv) = do atomically (writeTVar tv n)
                               -- we need two separate atomically block as
                               -- here we need to unblock for the helpers
                               -- to enters the gate
                               -- (look at last 2 lines of passGate)
                               atomically (do n_left <- readTVar tv
                                              check (n_left == 0))

data Group = MkGroup Cap (TVar (RemainingCap, Gate, Gate))

newGroup n = atomically (do g1 <- newGate n; g2 <- newGate n
                            tv <- newTVar (n, g1, g2)
                            return (MkGroup n tv))

-- assemble a group of n
joinGroup (MkGroup n tv) = atomically (do (n_left, g1, g2) <- readTVar tv
                                          check (n_left > 0)
                                          writeTVar tv (n_left-1, g1, g2)
                                          return (g1,g2))

-- wait for the group to assemble, then re-init the group
awaitGroup (MkGroup n tv) = do (n_left, g1, g2) <- readTVar tv
                               check (n_left == 0)
                               new_g1 <- newGate n; new_g2 <- newGate n
                               writeTVar tv (n,new_g1,new_g2)
                               return (g1,g2)


meetInStudy :: Int -> IO ()
meetInStudy id = putStr ("Elf " ++ show id ++ " meeting in the study\n" )

deliverToys :: Int -> IO ()
deliverToys id = putStr ("Reindeer " ++ show id ++ " delivering toys\n")


helper1 group do_task = do
                (in_gate, out_gate) <- joinGroup group
                passGate in_gate
                do_task
                passGate out_gate

elf1 gp id      = helper1 gp (meetInStudy id)
reindeer1 gp id = helper1 gp (deliverToys id)

main1 = do grp <- newGroup 1
           forkIO (elf1 grp 13)
           (in_gate, out_gate) <- atomically (awaitGroup grp)
           operateGate in_gate
           operateGate out_gate

-- for IO loop
forever :: IO () -> IO ()
forever act = forever' act 10
        where forever' act 0 = return ()
              forever' act n = do act
                                  forever' act (n-1)

-- delay between 1 μs to 1 sec
randomDelay = do waitTime <- getStdRandom (randomR (1, 1000000))
                 threadDelay waitTime

-- helper loop
elf gp id = forkIO (forever (do elf1 gp id
                                randomDelay))

reindeer gp id = forkIO (forever (do reindeer1 gp id
                                     randomDelay))

-- santa loop
-- this works but is clumsy: imagine if santa need to do very different things
-- for different choices he made (chooseGroup). For now he open and close gate
-- regardless of the choice
santa' :: Group -> Group -> IO ()
santa' elf_gp rein_gp = do putStr "--------\n"
                           (task, (in_gate, out_gate)) <- atomically (orElse
                             (chooseGroup rein_gp "deliver toys")
                             (chooseGroup elf_gp "meet in my study"))
                           putStr ("Ho! Ho! Ho! let's " ++ task ++ "\n")
                           operateGate in_gate
                           operateGate out_gate
                        where
                          chooseGroup :: Group -> String -> STM (String, (Gate, Gate))
                          chooseGroup gp task = do gates <- awaitGroup gp
                                                   return (task, gates)


-- whereas in this implementation, we can run arbitrary task after each choice
-- (the run task section)
santa :: Group -> Group -> IO ()
santa elf_gp rein_gp = do putStr "---------\n"
                          choose [(awaitGroup rein_gp, run "deliver toys"),
                                  (awaitGroup elf_gp, run "meet in my study")]
  where run task (in_gate, out_gate) = do putStr ("Ho! Ho! Ho! let's " ++ task ++ "\n")
                                          operateGate in_gate
                                          operateGate out_gate

choose :: [(STM a, a -> IO ())] -> IO ()
choose choices = do act <- atomically (foldr1 orElse actions)
                    act -- the task choosen
  where actions = [ do val <- guard -- this will block until being choosen
                       return (rhs val) -- returns the actions of type IO ()
                       | (guard, rhs) <- choices]



main = do putStrLn $ "number of cores: " ++ show numCapabilities
          elf_group <- newGroup 3
          sequence_ [ elf elf_group n | n <- [1..10]]
          rein_group <- newGroup 9
          sequence_ [ reindeer rein_group n | n <- [1..9]]
          forever (santa elf_group rein_group)
