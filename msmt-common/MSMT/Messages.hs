module MSMT.Messages where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad
import           Control.Monad.IO.Class
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

type MessageChan = TChan Message

isBadMessage :: Message -> Bool
isBadMessage (Warn _)  = True
isBadMessage (Error _) = True
isBadMessage _         = False

isDebug :: Message -> Bool
isDebug (Debug _) = True
isDebug _         = False

newMessageChannel :: MonadIO m => m MessageChan
newMessageChannel = liftIO $ newTChanIO

addMessage :: MonadIO m => MessageChan -> Message -> m ()
addMessage chan msg = liftIO $ atomically $ writeTChan chan msg

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
