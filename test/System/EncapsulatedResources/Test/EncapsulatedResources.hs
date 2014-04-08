-- | Utility functions for testing Resources.
module System.EncapsulatedResources.Test.EncapsulatedResources (
  TestLogger,
  TestLog(..),
  logToList,
  testResources
  ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import System.Timeout (timeout)
import Test.Hspec
import Prelude hiding (log)

-- | AUT includes.
import System.EncapsulatedResources

-- | The type of the logging action that the Resource-programs-under-test use
-- to report actions.
type TestLogger m a = a -> m ()

-- | The type of an action reading from a resource TestLogger.
newtype TestLog a = TestLog { unTestLog :: TChan a }

-- | Read all the items in the log to a list.
logToList :: TestLog a -> IO [a]
logToList (TestLog ch) = atomically $ go ch
  where
    go ch' = do
      mElem <- tryReadTChan ch
      case mElem of
        Just e -> (e :) <$> go ch'
        Nothing -> return []

-- | This action is useful for testing Resource programs.
testResources :: MonadIO m => Int -> (TestLogger m a -> ResourceM b) -> (TestLog a -> Expectation) -> Expectation
testResources time resAct assertion = do
  logChan <- atomically $ newTChan
  let logger = liftIO . atomically . writeTChan logChan
  resRes <- timeout (time * 1000) $ runResourceM def "testResources" (resAct logger)
  assertion $ TestLog logChan
  when (not $ isJust resRes) (expectationFailure "Tested resource timed out")
