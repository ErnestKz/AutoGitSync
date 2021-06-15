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


import           Control.Monad          (forever)
import           Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.Text              as T
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

gitPushFiles :: IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
gitPushFiles = createProcess $ shell "git add . && git commit -m \"test\" && git push"

data ReposConfig = ReposConfig RepoPath RepoBranch
  deriving Show
type RepoPath = T.Text
type RepoBranch = T.Text
type ConfigPath = String

readConfig :: IO [ReposConfig]
readConfig = parseConfig "/Users/ek/AutoGitSync/example-config"

parseConfig :: ConfigPath -> IO [ReposConfig]
parseConfig configPath = do
  fileLines <- fmap (T.lines . T.pack) $ openFile configPath ReadMode >>= hGetContents
  return $ (\(x:y:_) -> ReposConfig x y) . T.splitOn "," <$> fileLines

