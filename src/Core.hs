module Core where

import           Data.Time
import           System.Random

import           Reflex
import           Reflex.Host.Headless   (runHeadlessApp)


import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.List

type Users = [User]
data User = User UserName Bio Stats
type UserName = String
type Bio = String

newtype Stats = Stats [ClickEvent]
data ClickEvent = RedPageClick Time ButtonId
                | BluePageClick Time ButtonId
                deriving Show

type Time = UTCTime
type ButtonId = Integer


core :: IO ()
core = app


app = runHeadlessApp $ do
  (eExit, aExit) <- newTriggerEvent

  eClicks <- createEventStream
  performEvent $ fmap (liftIO . print) eClicks



  pure eExit -- this will wait for an aExit action


-- pretend user Alice and Bob are generating click events
-- red and blue

-- unfoldr  :: (b -> Maybe (a, b)) -> b -> [a]
-- uniformR :: (a, a) -> g -> (a, g)
randomNums :: Int -> [Int]
randomNums seed = unfoldr (Just . uniformR (1, 6)) $ mkStdGen seed


-- red page team has an api end point
-- blue page team has an api end point
a = mapM print $ randomNums 1

createEventStream :: (MonadIO m, TriggerEvent t m) => m (Event t ClickEvent)
createEventStream = do
  (events, eventTrigger) <- newTriggerEvent
  liftIO $ forkIO $ fireEvents eventTrigger
  return events

fireEvents :: (ClickEvent -> IO ()) -> IO ()
fireEvents trigger = forever $ do
    clickEvent <- newClickEvent'
    trigger clickEvent

newClickEvent' :: IO ClickEvent
newClickEvent' = fmap newClickEventRed getCurrentTime

newClickEventRed :: Time -> ClickEvent
newClickEventRed time = RedPageClick time 1
