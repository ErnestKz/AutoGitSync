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

<<<<<<< HEAD
-- import           Git
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Git.Libgit2


import           Data.Text

data FileChange = FileChange
  deriving Show
=======
import           Git.Libgit2
import           Git.Types


import           Control.Monad          (forever)
import           Control.Monad.Catch
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Git.Blob
>>>>>>> origin/main

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

<<<<<<< HEAD

-- currentRepo = openRepository defaultRep
-- a :: MonadGit r m => m r
-- a = getRepository
=======
testgit :: (MonadIO m, MonadMask m) => m LgRepo
testgit = openLgRepository defaultRepositoryOptions {repoPath = "/home/ek/TestRepo"}

git :: IO ()
git = do
  testRepo <- testgit
  runLgRepository testRepo $ do
    t <- newTreeBuilder Nothing
    -- a <- mtbBaseTreeOid t
    a <- mtbEntryCount t
    liftIO $ print a
    -- blob <- hashContents $ BlobString "."
    -- a  <- catBlob blob
    -- liftIO $ print t'
    return ()
  return ()
>>>>>>> origin/main



main = void $ withLibGitDo $
       withRepository lgFactory (fromText (pack "/tmp/blah.git")) $
           getRepository lgFactory
