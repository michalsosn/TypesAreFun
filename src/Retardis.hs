{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

module Retardis where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.Tardis

import Control.Monad.Trans.Step
import Control.Monad.Trans.Step.Concurrency

{-
instance (MonadTardis bw fw m, Applicative m, MonadFix m) => MonadTardis bw fw (StepT m) where
    getPast        = lift getPast
    getFuture      = lift getFuture
    sendPast   bw' = lift (sendPast bw')
    sendFuture fw' = lift (sendFuture fw')

instance (MonadStep m, Applicative m, MonadFix m) => MonadStep (TardisT bw fw m) where
    now  = lift . now
    step = lift . step
    run  = run
    finish = finish
    peekEnd = peekEnd

data DemoState = DemoState
    { counter :: Int
    }

demoState :: DemoState
demoState = DemoState 0

type Worker a = StepT (WriterT [String] (Tardis DemoState ())) a

worker :: String -> Worker ()
worker name = do
    let write :: String -> Worker ()
        write msg = tell ["[" ++ name ++ "] " ++ msg]
    write "Started."

    write "Getting value"
    val <- idle >> getsFuture counter
    write $ "Knows value: " ++ show val
    idle >> modifyBackwards (\s -> s { counter = val + 1 })
    write $ "Increased counter to " ++ show (val + 1)

workerReactor :: [Worker a] -> [Int] -> WriterT [String] (Tardis DemoState ()) ()
workerReactor (w:ws) (m:ms) = do
    w'   <- current $ runFor m w
    done <- finished w'
    let ws' = if done then ws else ws ++ [w']
    workerReactor ws' ms
workerReactor _ _  = return ()

runWorkers :: [String]
runWorkers = do
    let workers = fmap worker ["Johny", "Maciej", "Andrzej", "Wacław", "Jarosław", "Klapaucjusz"]
    runIdentity . flip evalTardisT (demoState, ()) . execWriterT $ workerReactor workers (take 100 $ cycle [2, 1])

demo :: IO ()
demo = forM_ runWorkers putStrLn
-}

demo = print "Hello"
