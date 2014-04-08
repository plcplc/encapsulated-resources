{-# LANGUAGE DeriveDataTypeable #-}

-- | This module defines the unit tests for Resources. Tests follow the general
-- pattern of defining a Resource-program that, through IO, is supposed to
-- deterministically produce a list of events, which are then asserted
-- afterwards. The 'testResources' action embodies this scheme.
module System.EncapsulatedResources.Test.Unit.EncapsulatedResources (resourcesSpecs, testResources) where

-- | Library includes.
import Control.Concurrent.STM
import Control.Monad
import Data.Map as M
import Data.Typeable
import System.IO (hGetContents)
import Test.Hspec
import Prelude hiding (log)

import System.EncapsulatedResources.Test.EncapsulatedResources

-- | AUT includes.
import System.EncapsulatedResources

data TestMsg = TestPing ResourceHandle | TestPong
  deriving (Eq, Show, Typeable)

data TestMsg2 = TestPing2 | TestPong2
  deriving (Eq, Show, Typeable)

data ResStarted = ResStarted ResourceHandle
  deriving (Eq, Show, Typeable)

-- | Specs for SelfContained Resources.
resourcesSpecs :: Spec
resourcesSpecs = do
  describe "SelfContained Resources" $ do
    it "knows its children" $ testResources 100 (\log -> do
      resource "parent" $ do
        resource "child" $ do
          recieve [Match (\ (TestPing _) -> return ())]
          ch <- resChildren
          log (M.size ch)
          destroyRes

        ch <- resChildren
        log (M.size ch)
        myRh <- currentRes
        send (ch M.! "child") (TestPing myRh)
        awaitChildren
        destroyRes

      awaitChildren
      destroyRes
      )
      (\logCh -> do
          log <- logToList logCh
          shouldBe log [1, 0])

    it "can send and recieve messages, like erlang" $ testResources 100 (\log -> do
      resource "child" $ do
        myRh <- currentRes
        resPar <- parentRes
        send resPar TestPing2
        send resPar (TestPing myRh)
        send resPar (TestPing myRh)
        destroyRes

      -- we can recieve in any order once recieved.
      recieve [Match (\TestPing2 -> log $ Right TestPong2)]
      recieve [Match (\(TestPing _) -> log $ Left TestPong)]
      destroyRes)

      (\logCh -> do
        log <- logToList logCh
        shouldBe log [Right TestPong2, Left TestPong])

    it "can lookup sibling resources" $ do

      testResources 100 (\log -> lookupableRes $ do

        resource "child1" $ lookupableRes $ do
          ch2 <- lookupRes "child2"
          myRh <- currentRes
          send ch2 (TestPing myRh)
          recieve [Match $ \ (TestPing ch2') -> log ("child2", ch2 == ch2')]
          destroyRes

        resource "child2" $ lookupableRes $ do
          ch1 <- lookupRes "child1"
          recieve [Match $ \ (TestPing ch1') -> log ("child1", ch1 == ch1')]
          myRh <- currentRes
          send ch1 (TestPing myRh)
          destroyRes

        awaitChildren
        destroyRes)

        (\log -> do
          logL <- logToList log
          shouldContain logL [("child1", True), ("child2", True)]
        )

    it "can lookup nested resources" $ do

      testResources 100 (\log -> lookupableRes $ do

        root <- currentRes

        resource "child1" $ lookupableRes $ do
          myRh <- currentRes
          send root (ResStarted myRh)
          recieve [Match $ \ TestPong -> return ()]

          ch2 <- lookupRes "child2"
          log ("lookupRes child2", True)
          send ch2 (TestPing myRh)
          recieve [Match $ \ (TestPing ch2') -> log ("child2", ch2 == ch2')]
          destroyRes

        resource "mediator" $ lookupableRes $ do
          resource "child2" $ lookupableRes $ do
            myRh <- currentRes
            send root (ResStarted myRh)
            recieve [Match $ \ TestPong -> return ()]

            ch1 <- lookupRes "child1"
            log ("lookupRes child1", True)
            recieve [Match $ \ (TestPing ch1') -> log ("child1", ch1 == ch1')]
            send ch1 (TestPing myRh)
            destroyRes

          awaitChildren
          destroyRes

        -- The test proceeds when everything has started up properly.
        rhx <- recieve [Match (\ (ResStarted rhx) -> return rhx)] -- Ready
        rhy <- recieve [Match (\ (ResStarted rhy) -> return rhy)] -- Set

        -- ... Go!
        send rhx TestPong
        send rhy TestPong

        awaitChildren
        destroyRes)

        (\log -> do
          logL <- logToList log
          shouldContain logL [("child1", True), ("child2", True)]
        )

    describe "The posix compat layer" $ do

      it "maps posix file api to underlying filesystem" $ do

        let file = "file"
        let contents = "foo"
        actualTV <- atomically $ newTVar "empty"

        runResourceM def "filetest" $ do
          rh <- fileRes file contents
          handle <- resAsFileHandle rh
          liftIO $ hGetContents handle >>= atomically . writeTVar actualTV
          destroyRes

        actual <- atomically $ readTVar actualTV
        shouldBe actual contents

