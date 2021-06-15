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

-- import           Git
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Git.Libgit2


import           Data.Text

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
-- a :: MonadGit r m => m r
-- a = getRepository



main = void $ withLibGitDo $
       withRepository lgFactory (fromText (pack "/tmp/blah.git")) $
           getRepository lgFactory
