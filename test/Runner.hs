-- | This module houses the customised hspec test runner.
module Runner (runSpec) where
import Control.Monad (unless)
import System.Exit (exitFailure)
import Test.Hspec (Spec)
import Test.Hspec.Runner (Config(..), defaultConfig, hspecWith, Summary(..))

-- | The hspec test runner.
runSpec :: Spec -> IO ()
runSpec specs = do
  r <- hspecWith conf specs
  unless (summaryFailures r == 0) exitFailure

-- | The customised configuration.
conf :: Config
conf = defaultConfig
