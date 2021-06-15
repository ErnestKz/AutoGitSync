{-# LANGUAGE OverloadedStrings #-}

module Core where

import           Reflex                 (PerformEvent (performEvent),
                                         Reflex (Event),
                                         TriggerEvent (newTriggerEvent))
import           Reflex.Host.Headless   (runHeadlessApp)


import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Monad
import           System.FSNotify        (startManager, watchDir, watchTree,
                                         withManager)
import qualified System.FSNotify        as FS


import           Control.Monad          (forever)
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           System.IO
import           System.Process

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

test = do
  (_, Just hout, _, _) <- createProcess ( proc "ls" [] ) { std_out = CreatePipe }
  hGetContents hout >>= print

gitPushFiles :: String -> IO ()
gitPushFiles repo = do
  createProcess $ shell "git add . && git commit -m \"test\" && git push"
  return ()
