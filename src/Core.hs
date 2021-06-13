{-# LANGUAGE OverloadedStrings #-}

module Core where

import           Reflex                 (PerformEvent (performEvent),
                                         Reflex (Event),
                                         TriggerEvent (newTriggerEvent))
import           Reflex.Host.Headless   (runHeadlessApp)

import           Control.Concurrent     (forkIO, threadDelay)
import           System.FSNotify        (startManager, watchDir, watchTree,
                                         withManager)
import qualified System.FSNotify        as FS

import           Git.Types


import           Control.Monad          (forever)
import           Control.Monad.IO.Class (MonadIO, liftIO)


data FileChange = FileChange
  deriving Show

core :: IO ()
core = runHeadlessApp $ do
  (eExit, aExit) <- newTriggerEvent
  fileChanges <- createWatcherEventStream
  performEvent $ fmap (liftIO . print) fileChanges
  pure eExit -- this will wait for an aExit action

createWatcherEventStream :: (MonadIO m, TriggerEvent t m) => m (Event t FS.Event)
createWatcherEventStream  = do
  (events, eventTrigger) <- newTriggerEvent
  liftIO $ do
    mgr <- startManager
    watchTree mgr "." (const True) eventTrigger
  return events


-- currentRepo = openRepository defaultRep



