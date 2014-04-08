{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | This module houses an experiment for a grander vision of programs and data
-- as self contained resources with proper encapsulation and transparency
-- properties. This concerete instance is a proof of concept that just contains
-- what's necessary to distribute programs portably on all of Haskell's
-- supported platforms (Linux, Mac, Windows) as self contains as technically
-- possible.
--
-- It's a bit like Plan9's 'everything is a file' idea, except that rather than
-- identifying everything in the system with files (where it necessarily is
-- meaningfull to be able to read, write and store data), we slightly
-- generalise the identity of everything to an abstract 'Resource'.
--
-- 'Resource's are semantically identical to nested virtual machines. Thus, an
-- inner Resource is completely at the mercy of its containing Resource, and
-- passing of messages between Resources depends on the existance of a chain of
-- resources willing to route.
--
-- In the longer run I imagine implementing IPC between resources, an
-- equivalent C library, a method of wrapping POSIX progams as Resources,
-- network transparent resource manipulation, cli resource shell/tools, a
-- filesystem driver for POSIX access to resources, a linux distribution that
-- wraps everything as resources and eventually evolves to a minikernel with a
-- resource-based POSIX compatibilty layer, world domination.

module System.EncapsulatedResources (

  -- * Types
  ResourceHandle,
  ResourceM,

  -- * Functions for creating Resources
  dirRes,
  fileRes,
  pipeRes,
  sockRes,
  resource,

  -- * Functions for manipulating Resources
  destroyRes,
  destroyChildRes,
  runResourceM,

  -- * Primitive operations
  currentRes,
  getName,
  parentRes,
  parR,
  send,
  sendDyn,
  sendIO,
  recieve,
  resChildren,
  resParent,
  Matcher (..),
  awaitChildren,

  -- * Standard library of operations
  lookupRes,
  lookupableRes,
  getBoundSock,
  getConnectedSock,
  proxyRes,
  serveLookup,
  getDirPath,

  -- * POSIX/OS compatibility
  -- $posixnotes
  resAsFileHandle,
  posixCompatExitFailure,

  -- * Useful re-exports
  def,
  liftIO

  ) where

import Control.Applicative
import Control.Exception (catch, Exception, throwTo, throw)
import Control.Monad (forever, when)
import Control.Monad.Trans.State (evalStateT, get, StateT)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO, forkFinally, myThreadId, ThreadId, threadDelay )

import Control.Concurrent.STM
  ( atomically, newTChan, STM, TVar, newEmptyTMVar, newTVar, putTMVar,
  readTChan, readTVar, takeTMVar, tryTakeTMVar, TChan, retry, unGetTChan, writeTChan, writeTVar )

import Data.Default (Default(..))
import Data.Dynamic (Dynamic, dynTypeRep, fromDyn, toDyn)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Traversable (traverse)
import Data.Typeable

import Network.Socket ( bind, connect, defaultProtocol, Family(..), listen, SocketType(..), SockAddr(..), socket, Socket )

import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.Environment (getProgName)
import System.Exit (exitFailure)
import System.IO ( Handle, IOMode(..), openFile )
import System.IO.Error ( catchIOError )

import System.Posix.Files
  ( createNamedPipe, unionFileModes, nullFileMode, ownerReadMode,
  ownerWriteMode, groupReadMode, groupWriteMode )

import System.Posix.Temp (mkdtemp)

-- | The data type of Resources. This is purposefully kept abstract. Anything
-- it contains is entirely implementation details.
data Resource = Resource {

  -- | It has a name.
  resName :: !String,

  -- | It needs to account for the resources created below it.
  resChildResources :: !(M.Map String (TVar Resource)),

  -- | It has some exclusive space in the filesystem. (Absolute path)
  resFsSpace :: !FilePath,

  -- | It has an inbox for communication. (In a proper OS implementation this would be untyped)
  resInbox :: !ResChan,

  -- | It knows its parent resource
  resParent :: !ResChan,

  -- | It has a Haskell thread associated with it.
  resThreadId :: !ThreadId,

  -- | For posix compat, it might need finalizers to properly destroy.
  resFinalizer :: !(IO ())
  }

-- | The type of resource inbox channels
type ResChan = TChan Dynamic

-- | Craft a new Resource. (internal)
newRes :: String -> FilePath -> ResChan -> ThreadId -> IO () -> STM (TVar Resource)
newRes name fsSpace parentChan tId finalizer = do
  inboxChan <- newTChan
  newTVar $ Resource name M.empty fsSpace inboxChan parentChan tId finalizer

--newtype ResourceM a = ResourceM { unResourceM :: StateT Resource IO a }
type ResourceM a = StateT (TVar Resource) IO a

-- | Create a Resource with a POSIX directory compatible interface.
-- (For now we just cheat and exploit that a vanilla 'resource' is respresented by a directory.)
dirRes :: String -> ResourceM () -> ResourceM ResourceHandle
dirRes name contents = resource name
  (parR contents
    (do
      resTV <- get
      path <- ((++name) . (++"/") . resFsSpace) <$> (liftIO $ atomically $ readTVar resTV)
      liftIO $ createDirectoryIfMissing True path
      forever $ recieve [Match $ \ (ReqDirPath rh) -> send rh (RespDirPath path)]
      ))

-- | Get a FilePath for a Directory Resource.
getDirPath :: ResourceHandle -> ResourceM FilePath
getDirPath rh = do
  myRh <- currentRes
  send rh (ReqDirPath myRh)
  recieve [Match $ \ (RespDirPath fp) -> return fp]

-- | A datatype representing the protocol used by posix directory resources.
-- Typically just encapsulated by 'getDirPath'.
data PosixDirPath = ReqDirPath ResourceHandle | RespDirPath FilePath
  deriving Typeable

-- | Create a Resource with a POSIX file compatible interface.
fileRes :: String -> String -> ResourceM ResourceHandle
fileRes name contents = resource name $ do

  -- Get to the underlying filesystem.
  resTV <- get
  fileH <- liftIO $ do
    curRes <- atomically $ readTVar resTV
    let dir = resFsSpace curRes
    let fileFP = dir ++ "/" ++ name

    -- Create the file.
    createDirectoryIfMissing True dir
    writeFile fileFP contents

    openFile fileFP ReadWriteMode

  -- We support just one operation: get the file handle.
  forever $ recieve $ serveLookup ++ [
    Match $ \(ReqFileHandle rh) -> send rh (RespFileHandle fileH) ]

-- | A data type that specifies how the 'Resources' implementation should
-- interact with the underlying POSIX operating system. The application that
-- uses the Resources framework should not rely on these settings.
data ResourcesPosixSettings = RPS {

  -- | The root directory of the process resource. If 'Nothing' the root is
  -- taken as whatever glibc's 'mkdtemp' function gives.
  rpsRootDir :: Maybe FilePath

  }

instance Default ResourcesPosixSettings where
  def = RPS Nothing

data PosixFileCompatMsgs = ReqFileHandle ResourceHandle | RespFileHandle Handle
  deriving Typeable

-- | An abstract type representing a reference to a Resource.
newtype ResourceHandle = RH ResChan
  deriving (Eq, Typeable)

instance Show ResourceHandle where
  show _ = "<ResourceHandle>"

-- | Recieve matchers.
data Matcher b =
  forall a. Typeable a => Match (a -> ResourceM b)
  | MatchDyn (Dynamic -> ResourceM b) -- ^ Wildcard matching

-- | Initialize the mechanics for interacting with resources in this process
-- resource.
runResourceM :: ResourcesPosixSettings -> String -> ResourceM a -> IO ()
runResourceM settings name act = do
  rootDir <- maybe
    (getProgName >>= mkdtemp)
    (pure)
    (rpsRootDir settings)
  rootChan <- atomically $ newTChan
  tId <- myThreadId
  rootRes <- atomically $ newRes name rootDir rootChan tId (return ())
  catch (evalStateT act rootRes >> return ())
    (\DestroyResourceException -> tryRmDirRec rootDir)

-- | Just try to remove a dir, and disregard all errors.
tryRmDirRec :: FilePath -> IO ()
tryRmDirRec dir = System.IO.Error.catchIOError (removeDirectoryRecursive dir) (\_ -> return ()) -- (Just ignore errors)

-- | Delete (forcibly deallocate) a Resource.
destroyRes :: ResourceM ()
destroyRes = throw DestroyResourceException

-- | Destroy a child of the current resource.
destroyChildRes :: String -> ResourceM ()
destroyChildRes = error "Not implemented: destroyChildRes"

-- | Get the current resource.
currentRes :: ResourceM ResourceHandle
currentRes = (RH . resInbox) <$> (get >>= (liftIO . atomically . readTVar))

-- | Create a Resource with a POSIX fifo-file compatible interface. (Currently a POSIX-compat only)
pipeRes :: String -> ResourceM ResourceHandle
pipeRes name = resource name $ do

  -- Get to the underlying filesystem.
  resTV <- get
  pipeH <- liftIO $ do
    curRes <- atomically $ readTVar resTV
    let dir = resFsSpace curRes
    let pipeFP = dir ++ "/" ++ name

    -- Create the pipe.
    createDirectoryIfMissing True dir
    createNamedPipe pipeFP
      (foldr unionFileModes nullFileMode
        [ownerReadMode, ownerWriteMode, groupReadMode, groupWriteMode])

    openFile pipeFP ReadWriteMode

  -- We support just one operation: get the file handle.
  forever $ recieve $ serveLookup ++ [Match $ \(ReqFileHandle rh) -> send rh (RespFileHandle pipeH)]

-- | Create a Resource with a unix socket compatible interface. (Currently a POSIX-compat only)
sockRes :: String -> ResourceM ResourceHandle
sockRes name = resource name $ do

  -- Get to the underlying filesystem.
  resTV <- get
  (sockFP, sock) <- liftIO $ do
    curRes <- atomically $ readTVar resTV
    let dir = resFsSpace curRes
    let sockFP = (SockAddrUnix $ dir ++ "/" ++ name)

    -- Create the socket.
    createDirectoryIfMissing True dir
    sock <- socket AF_UNIX Stream defaultProtocol
    bind sock sockFP
    listen sock 100
    return (sockFP, sock)

  -- We support two operations: getting a bound socket, and a connected socket.
  -- There can only be one bound socket, but an arbitrary number of connected
  -- sockets.
  forever $ recieve $ serveLookup ++ [
    Match $ \req -> case req of
      ReqBoundSock rh -> send rh (RespSock sock)
      ReqConnectedSock rh -> do
        sockC <- liftIO $ socket AF_UNIX Stream defaultProtocol
        liftIO $ connect sockC sockFP
        send rh (RespSock sockC)
    ]

-- | Messages for unix-socket resources. You can request a bound (serverside)
-- socket or a connected (clientside) socket end. There is only a single bound
-- socket end, and every request for a connected end gets a fresh 'Socket'.
data UnixSockReq = ReqBoundSock ResourceHandle | ReqConnectedSock ResourceHandle
  deriving Typeable

data UnixSockResp =  RespSock Socket
  deriving Typeable

-- | Get a bound unix socket associated with a socket resource.
getBoundSock :: ResourceHandle -> ResourceM Socket
getBoundSock rh = do
  myRh <- currentRes
  send rh (ReqBoundSock myRh)
  recieve [Match $ \(RespSock sock) -> return sock]

-- | Get a connected unix socket associated with a socket resource.
getConnectedSock :: ResourceHandle -> ResourceM Socket
getConnectedSock rh = do
  myRh <- currentRes
  send rh (ReqConnectedSock myRh)
  recieve [Match $ \(RespSock sock) -> return sock]

-- | Create a proxy for another resource. All messages recieved are relayed to
-- the given ResourceHandle. Useful for composing resources.
proxyRes :: String -> ResourceHandle -> ResourceM ResourceHandle
proxyRes name rh = resource name $ forever $ recieve [MatchDyn $ sendDyn rh]

-- | Consume the top of the Resource inbox. This mimics the Erlang primitive with the same name.
recieve :: [Matcher b] -> ResourceM b
recieve matchers = do
  resTV <- get
  (matcher, matched) <- liftIO $ atomically $ do
    res <- readTVar resTV
    getFirstMatch matchers (resInbox res)

  applyMatcher matcher matched

  where

    getFirstMatch :: [Matcher b] -> ResChan -> STM (Matcher b, Dynamic)
    getFirstMatch matchers' =
      cherryPickTChan (\ dyn ->
        let First matcher = foldr mappend (First Nothing)
                            (map (\m -> if matchTypeRep dyn m
                              then First (Just m)
                              else First Nothing) matchers')
        in do
            m <- matcher
            return (m, dyn)
        ) []

    matchTypeRep :: Dynamic -> Matcher b -> Bool
    matchTypeRep dyn (Match (_ :: a -> ResourceM b)) = dynTypeRep dyn == typeOf (undefined :: a)
    matchTypeRep _ (MatchDyn _) = True

    applyMatcher :: Matcher b -> Dynamic -> ResourceM b
    applyMatcher (Match fn) dyn = fn (fromDyn dyn (error "Internal type error - impossible"))
    applyMatcher (MatchDyn fn) dyn = fn dyn

    cherryPickTChan :: (a -> Maybe b) -> [a] -> TChan a -> STM b
    cherryPickTChan p skipped tch = do
      x <- readTChan tch
      case p x of
        Just res -> do
          mapM (unGetTChan tch) skipped
          return res
        Nothing -> cherryPickTChan p (x:skipped) tch

-- | Send data to a Resource.
send :: Typeable a => ResourceHandle -> a -> ResourceM ()
send (RH chan) dat = do
  liftIO $ atomically $ writeTChan chan $ toDyn dat

-- | Send Dynamic data to a Resource. Recievers can match the contained type
-- rather than just the Dynamic.
sendDyn :: ResourceHandle -> Dynamic -> ResourceM ()
sendDyn (RH chan) dat = do
  liftIO $ atomically $ writeTChan chan dat

-- | Construct an IO action to send messages to a given Resource.
sendIO :: Typeable a => ResourceHandle -> ResourceM (a -> IO ())
sendIO (RH chan) = do
  return $ \dat -> atomically $ do
    writeTChan chan $ toDyn dat

-- | Exception to destroy (remove every persisted trail of the resource and child resources) a resource.
data DestroyResourceException = DestroyResourceException
  deriving (Eq, Show, Typeable)

instance Exception DestroyResourceException

-- | Create a new Resource (machine). The given action is the program that the
-- Resource implements. Resources are persistent until destroyed, either
-- volunteerily or by their parent.
resource :: String -> ResourceM a -> ResourceM ResourceHandle
resource name act = do
  resTV <- get
  liftIO $ do
    childHandleMV <- atomically $ newEmptyTMVar

    forkIO $ do
      tId <- myThreadId
      freshResTV <- atomically $ do
        parRes <- readTVar resTV
        freshResTV <- newRes name ((resFsSpace parRes) ++ "/" ++ (resName parRes)) (resInbox parRes) tId (return ())

        -- Register with parent.
        writeTVar resTV $ parRes { resChildResources = M.insert name freshResTV (resChildResources parRes) }
        readTVar freshResTV >>= (putTMVar childHandleMV) . RH . resInbox
        return freshResTV

      -- Run the action. (need better way wait for exceptions.)
      catch (evalStateT act freshResTV >> forever (threadDelay 10000000))
        (\DestroyResourceException -> do

          -- Remove from parent
          (finalizer, subResIds) <- atomically $ do
            parRes <- readTVar resTV
            writeTVar resTV $ parRes { resChildResources = M.delete name (resChildResources parRes) }

            freshRes <- readTVar freshResTV
            subResIds <- traverse (\resTV' -> resThreadId <$> readTVar resTV') (resChildResources freshRes)
            return (resFinalizer freshRes, subResIds)

          -- Kill off children
          traverse (flip throwTo DestroyResourceException) subResIds

          -- Run finalizers
          finalizer

          return ()
        )

    -- Return a handle for the new resource.
    atomically $ takeTMVar childHandleMV

-- | wait until all children have exited.
awaitChildren :: ResourceM ()
awaitChildren = do
  resTV <- get
  liftIO $ atomically $ do
    res <- readTVar resTV
    when (M.size (resChildResources res) > 0) retry

-- $posixnotes
--
-- Currently, with respect to filesystem-based resources we try to map the
-- Resource hierachy as naturally as possible to the directory-file structure
-- of regular filesystems.
--
-- Thus, a resource "Res" that contains sub-resources that are files has a
-- directory "Res(.dir)?" associated with it. A file-resource named "foo" becomes
-- a file named "foo" in the directory associated with the parent resource.

-- | Lookup a posix-filelike resource and give a posix file handle.
resAsFileHandle :: ResourceHandle -> ResourceM Handle
resAsFileHandle rh = do
  myRh <- currentRes
  send rh (ReqFileHandle myRh)
  recieve [Match (\(RespFileHandle fh) -> return fh)]

-- | Lookup a Resource from its name, by enquiring sub-resources and the parent
-- resource.
lookupRes :: String -> ResourceM ResourceHandle
lookupRes name = do
  -- Ask everyone we're connected to
  me <- currentRes
  par <- parentRes
  children <- M.elems <$> resChildren
  mapM (flip send (me, LookupResReq (me, name))) $ par:children

  -- Wait for a response
  recieve [Match (\(LookupResResp rh) -> return rh)]

-- | Matcher to enable routing of resource lookup requests.
serveLookup :: [Matcher ()]
serveLookup = [Match
  (\ (asker, LookupResReq (tellRes, name)) -> do
    isMe <- (name ==) <$> getName
    if isMe then do
      currentRes >>= (send tellRes) . LookupResResp
    else do
      -- Ask everyone except the resource that asked us.
      par <- parentRes
      children <- M.elems <$> resChildren
      myRh <- currentRes
      mapM_ (flip send (myRh, LookupResReq (tellRes, name))) (filter (/= asker) $ par:children)
    )
  ]

-- | Make the current resource lookup-able by running 'forever $ recieve serveLookup' in parallel.
lookupableRes :: ResourceM a -> ResourceM a
lookupableRes = (fmap snd) . (parR (forever $ recieve serveLookup))

-- | Run two resource actions in parallel, return when both are done. Uncaught
-- exceptions in either branch are propagated upwards, and both are killed.
parR :: ResourceM a -> ResourceM b -> ResourceM (a,b)
parR actA actB = do
  resTV <- get
  liftIO $ do

    (actAM, actBM) <- atomically $ (,) <$> newEmptyTMVar <*> newEmptyTMVar

    thdA <- forkFinally (evalStateT actA resTV)
      (atomically . (putTMVar actAM))

    thdB <- forkFinally (evalStateT actB resTV)
      (atomically . (putTMVar actBM))

    res <- atomically $ do
      a <- tryTakeTMVar actAM
      b <- tryTakeTMVar actBM
      -- Either both are successful, or one has failed and we should kill the other, or both have failed.
      case (a,b) of
        (Just a', Just b') -> case (a', b') of -- Both have finished
          (Right a'', Right b'') -> return $ Right (a'', b'')
          (Left exnA, _) -> return $ Left (exnA, Nothing)
          (_,         Left exnB) -> return $ Left (exnB, Nothing)

        (Just a', Nothing) -> case a' of    -- A has finished
          Left exnA -> return $ Left (exnA, Just thdB) -- Re-throw to thdB
          _ -> retry
        (Nothing, Just b') -> case b' of    -- B has finished
          Left exnB -> return $ Left (exnB, Just thdA) -- Re-throw to thdA
          _ -> retry
        (Nothing, Nothing) -> retry

    case res of
      Left (exn, mThd) -> do
        maybe (return ()) (flip throwTo exn) mThd
        throw exn
      Right (a,b) -> return (a,b)

-- | Basic resources actions / standard lib.
data LookupResReq = LookupResReq (ResourceHandle, String)
  deriving (Typeable)
-- | Basic resources actions / standard lib.
data LookupResResp = LookupResResp ResourceHandle
  deriving (Typeable)

-- | Get the name of the Resource.
getName :: ResourceM String
getName = resName <$> (get >>= liftIO . atomically . readTVar)

-- | Get the parent Resource of the current Resource.
parentRes :: ResourceM ResourceHandle
parentRes = (RH . resParent) <$> (get >>= (liftIO . atomically . readTVar))

-- | Get a map of the child Resources of the current Resource.
resChildren :: ResourceM (M.Map String ResourceHandle)
resChildren = get >>= \resTV -> liftIO $ atomically $ do
  res <- readTVar resTV
  traverse ((RH . resInbox <$>) . readTVar) (resChildResources res)

-- | Halt the Resource and emit an errorneous exit status (only useful if the
-- Resource is the root resource running in a posix-compatible setting)
posixCompatExitFailure :: ResourceM ()
posixCompatExitFailure = liftIO $ exitFailure

