module MSMT.Util.Workers where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBQueue
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import qualified Data.Map                       as M

data WorkingStatus a = Running  a
                   | Finished

data WorkingSet a = WorkingSet
  { queue      :: TBQueue (WorkingStatus a)
  , workers    :: TVar (M.Map Int (Async ()))
  , totalItems :: TVar Int }

newWorkingSet :: (MonadIO m) => Int -> m (WorkingSet a)
newWorkingSet i = do
  queue   <- liftIO $ newTBQueueIO i
  workers <- liftIO $ newTVarIO M.empty
  total   <- liftIO $ newTVarIO 0
  return $ WorkingSet queue workers total

addWork :: (MonadIO m) => WorkingSet a -> a -> m ()
addWork (WorkingSet queue _ _) v = liftIO $ atomically $ writeTBQueue queue (Running v)

finish :: (MonadIO m) => WorkingSet a -> m ()
finish (WorkingSet queue _ _) = liftIO $ atomically $ writeTBQueue queue Finished

work :: (MonadIO m) => Int -> WorkingSet a -> (Int -> a -> IO ()) -> m ()
work i set f = mapM_ (\i -> spawnWorker i set (f i) ) [1..i]

spawnWorker :: (MonadIO m) => Int -> WorkingSet a -> (a -> IO ()) -> m ()
spawnWorker i (WorkingSet queue ref tref) f = do
  thread <- liftIO $ async workingLoop
  liftIO $ atomically $ modifyTVar ref (M.insert i thread)
  where
    workingLoop = do
      next <- atomically $ do
        modifyTVar tref (+ 1)
        readTBQueue queue

      case next of
        Running a -> f a >> workingLoop
        Finished  -> liftIO $ atomically $ do
            writeTBQueue queue Finished
            modifyTVar ref (M.delete i)


waitForWorkDone :: (MonadIO m) => WorkingSet a -> m Int
waitForWorkDone (WorkingSet _ ref tref) = do
  workers <- liftIO $ atomically $ readTVar ref
  liftIO $ mapM_ wait (M.elems workers)
  liftIO $ readTVarIO tref















--work :: (MonadIO m) => Int -> WorkingSet a -> (a -> m ())-> m
