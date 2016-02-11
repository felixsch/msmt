module MSMT.Messages where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad
import           System.IO

import           MSMT.Util


data Message = Info String
             | Warn String
             | Error String
             | Debug String
             | Say String

instance Show Message where
  show (Say str)     = str
  show (Info str)    = "(II): " ++ str
  show (Warn str)    = "(WW): " ++ str
  show (Error str)   = "(EE): " ++ str
  show (Debug str)   = "(DD): " ++ str

isBadMessage :: Message -> Bool
isBadMessage (Warn _)  = True
isBadMessage (Error _) = True
isBadMessage _         = False

isDebug :: Message -> Bool
isDebug (Debug _) = True
isDebug _         = False



type MessageChan = TChan Message


newMessageChannel :: MonadIO m => m MessageChan
newMessageChannel = liftIO $ newTChanIO

stdSubscriber :: MonadIO m => Bool -> MessageChan -> m ()
stdSubscriber verbose chan = do
  dupc <- liftIO $ atomically $ dupTChan chan
  fork_ $ forever $ do
    message <- atomically $ readTChan dupc
    showMessage message
  where
    showMessage msg
      | isDebug msg && verbose                = print msg
      | not (isDebug msg) && isBadMessage msg = hPrint stderr msg
      | not (isDebug msg)                     = print msg
      | otherwise                             = return ()
