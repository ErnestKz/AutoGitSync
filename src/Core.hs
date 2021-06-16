{-# LANGUAGE OverloadedStrings #-}

module Core where

import           Reflex                 (PerformEvent (performEvent),
                                         Reflex (Event),
                                         TriggerEvent (newTriggerEvent),
                                         ffilter)
import           Reflex.Host.Headless   (runHeadlessApp)

import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Monad          (forever, void, (>=>))
import           Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.Text              as T

import           System.FSNotify        (startManager, watchDir, watchTree)
import qualified System.FSNotify        as FS
import           System.IO
import           System.Process

import           GHC.Base
import           System.Directory
import           System.Exit
import           System.FilePath.Posix


newtype RepoRoot = RepoRoot FilePath
  deriving Show

newtype RepoSyncBranch = RepoSyncBranch String
  deriving Show

data RepoFileChange = RepoFileChange FS.Event RepoRoot RepoSyncBranch
  deriving Show

-- I can see why you might want to use newtype rather than type
-- When you decide if you want to change the order of the types, the compiler might not complain even if the code
-- is incorrect, since otherwise you'd be using the data type specific functions which the compiler will tell you about

repoFileChange :: RepoSyncBranch -> RepoRoot -> FS.Event -> RepoFileChange
repoFileChange syncBranch repoRoot fileEvent = RepoFileChange fileEvent repoRoot syncBranch

getEventPath :: RepoFileChange -> FilePath
getEventPath (RepoFileChange fileEvent _ _) = FS.eventPath fileEvent

getRepoRootPath :: RepoFileChange -> FilePath
getRepoRootPath (RepoFileChange _ (RepoRoot repoPath) _) = repoPath

core :: IO ()
core = runHeadlessApp $ do
  (eExit, aExit) <- newTriggerEvent
  repos <- liftIO readConfig
  fileChanges <- createWatcherEventStream repos
  performEvent $ fmap (liftIO . checkAndPush) fileChanges
  liftIO $ forkIO $ do
    forever $ do
      pullChanges repos
      threadDelay 60000000
  pure eExit -- this will wait for an aExit action

createWatcherEventStream :: (MonadIO m, TriggerEvent t m) => [ReposConfig] -> m (Event t RepoFileChange)
createWatcherEventStream repos = do
  (events, eventTrigger) <- newTriggerEvent
  liftIO $ do
    mgr <- startManager
    mapM_
      (\(ReposConfig repoRoot@(RepoRoot repoPath) syncBranch) -> watchTree mgr repoPath ignoreDefault (eventTrigger . repoFileChange syncBranch repoRoot))
      repos
  return events

inGitFolder :: FS.Event -> Bool
inGitFolder fileEvent = elem ".git/" $ splitPath $ FS.eventPath fileEvent

isEmacsTmpFile :: FS.Event -> Bool
isEmacsTmpFile fileEvent = take 2 (takeBaseName $ FS.eventPath fileEvent) == ".#"

ignoreDefault :: FS.Event -> Bool
ignoreDefault fileEvent = not (inGitFolder fileEvent || isEmacsTmpFile fileEvent)

inGitIgnore :: RepoFileChange -> IO Bool
inGitIgnore (RepoFileChange fileEvent (RepoRoot repoPath) _) = do
  (_, _, _, processHandle) <- createProcess (shell ("git check-ignore " ++ FS.eventPath fileEvent)) {cwd = Just repoPath}
  exitCode <- waitForProcess processHandle
  return $ case exitCode of
    ExitSuccess   ->  True
    ExitFailure _ ->  False

inSyncBranch :: RepoRoot -> RepoSyncBranch -> IO Bool
inSyncBranch (RepoRoot repoPath) (RepoSyncBranch syncBranch) = do
 currentBranch <- readCreateProcess ((shell "git branch --show-current") {cwd = Just repoPath}) ""
 let currentBranchWithoutNewline = init currentBranch
 return $ syncBranch == currentBranchWithoutNewline

gitPushFiles :: RepoFileChange -> IO ()
gitPushFiles (RepoFileChange fileEvent (RepoRoot repoPath) _ ) = void (createProcess (shell "git add . && git commit -m \"Test\" && git push") {cwd = Just repoPath})

checkAndPush :: RepoFileChange -> IO ()
checkAndPush fileChange@(RepoFileChange _ repoRoot syncBranch) = do
  gitIgnore <- inGitIgnore fileChange
  syncBranch <- inSyncBranch repoRoot syncBranch
  when (not gitIgnore && syncBranch) $ do
    gitPushFiles fileChange


pullChanges :: [ReposConfig] -> IO ()
pullChanges repos = do
  let pull (ReposConfig repoRoot@(RepoRoot rootPath) syncBranch) = do
        validBranch <- inSyncBranch repoRoot syncBranch
        when validBranch $ void $ createProcess (shell "git pull") {cwd = Just rootPath}
  mapM_ pull repos

data ReposConfig = ReposConfig RepoRoot RepoSyncBranch
  deriving Show

newtype ConfigPath = ConfigPath FilePath
  deriving Show

readConfig :: IO [ReposConfig]
readConfig = parseConfig "/home/ek/AutoGitSync/example-config"

parseConfig :: FilePath -> IO [ReposConfig]
parseConfig configPath = do
  fileLines <- lines <$> (openFile configPath ReadMode >>= hGetContents)
  return $ (\(x:y:_) -> ReposConfig (RepoRoot x) (RepoSyncBranch y)) . split "," <$> fileLines

split :: String -> String -> [String]
split c s = T.unpack <$> T.splitOn (T.pack c) (T.pack s)
