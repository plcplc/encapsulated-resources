-- | This is the main module of the unit test suite.
module Main where

import System.EncapsulatedResources.Test.Unit.EncapsulatedResources (resourcesSpecs)
import Runner (runSpec)

-- | Run the unittest test suite.
main :: IO ()
main = runSpec
  resourcesSpecs
