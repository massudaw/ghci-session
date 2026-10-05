{-# LANGUAGE ScopedTypeVariables #-}

-- | __Run a server as a forked CHILD of a warm GHCi session.__
--
-- Serving from a thread of the repl (@forkIO@) has two costs: the thread runs the code it was started
-- with, so serving new code needs a repl restart rather than a @:reload@, and a server that wedges takes
-- the session with it. 'forkProcess' gives the server its own PID while inheriting the session's
-- already-loaded module graph copy-on-write. The reload cycle becomes: @:reload@ the parent, stop the old
-- child, fork a fresh one. @ghci-session@ drives this module for a target that declares a @server@.
--
-- == What it costs
--
-- A freshly forked child that does nothing costs a few MB; once it has built its state and is serving it
-- costs its own heap, because every page it dirties stops being shared. Measured on one server: 298 MB as
-- a standalone executable, 494 MB forked from a warm repl, ~950 MB as a repl of its own. So a fork is about
-- half a dedicated session: worth it when the server should track code edits, not when you only want to run it.
--
-- == Two rules
--
-- * The child is a fork WITHOUT an exec. On macOS, libraries that are not fork-safe (Accelerate/LAPACK,
--   GSL, most GUI frameworks) crash in it, silently. Do that work in the parent before the fork (the
--   target's @prefork@) and let the child serve the result.
-- * The child's stdout and stderr go to its log file, never the repl's: the repl's stdout is the channel
--   the session speaks its protocol on.
module GHC.Hygiene.Zygote
  ( ZygoteSpec(..)
  , ZygoteChild(..)
  , defaultZygoteSpec
  , zygoteSpec
  , zygoteFork
  , zygoteAlive
  , zygoteStop
  , zygoteReap
    -- * State handover
  , setHandoverExporter
  , handoverInPath
  , handoverEnvOut
  , handoverEnvIn
  ) where

import qualified Data.ByteString.Lazy as BL
import Control.Concurrent (threadDelay)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (void, when)
import Control.Exception (SomeException, try)
import System.Environment (setEnv, withArgs)
import System.Exit (ExitCode (..))
import System.Posix.IO
  ( OpenMode (..), closeFd, defaultFileFlags, dupTo, openFd, stdError, stdInput
  , stdOutput, creat, trunc )
import System.Posix.Process
  ( ProcessStatus, createSession, exitImmediately, forkProcess, getProcessStatus )
import System.Posix.Signals
  ( Handler (..), installHandler, signalProcess, sigKILL, sigTERM )
import System.Posix.Types (ProcessID)

-- | Env var naming where a dying child should WRITE its state.
handoverEnvOut :: String
handoverEnvOut = "GHS_HANDOVER_OUT"

-- | Env var naming where a starting child should READ state from.
--
-- Two variables, not one, because they answer different questions and the
-- answer to the second must be allowed to be "nowhere". A cold start and a
-- hot reload differ only in whether this is set, so the parent decides which
-- kind of start this is and the child does not have to guess from a file that
-- may simply be stale.
handoverEnvIn :: String
handoverEnvIn = "GHS_HANDOVER_IN"

-- | How the RUNNING server exports its state, registered by the server itself.
--
-- A global, and deliberately. The exporter closes over state that does not
-- exist until the server has built it -- a session, a compile, an IORef of
-- live books -- so it cannot be passed at fork time, which is the only other
-- place the child's code is reachable from. The signal handler needs to find
-- it at an instant when nothing is on the stack to hand it over.
{-# NOINLINE handoverExporter #-}
handoverExporter :: IORef (Maybe (IO BL.ByteString))
handoverExporter = unsafePerformIO (newIORef Nothing)

-- | Register how to export this process's state. Call it once the state
-- exists; until then a SIGTERM simply exits, as it did before.
setHandoverExporter :: IO BL.ByteString -> IO ()
setHandoverExporter f = writeIORef handoverExporter (Just f)

-- | Where this process should read inbound state from, if anywhere.
--
-- The server calls this at startup. 'Nothing' is the ordinary case and means
-- a cold start; it is not an error and must not be reported as one.
handoverInPath :: IO (Maybe FilePath)
handoverInPath = lookupEnv handoverEnvIn

-- | Write the envelope, if this process has one and was told where to put it.
--
-- Failure is swallowed on purpose. This runs while the process is being asked
-- to die: a handover that cannot be written costs the NEXT process its warm
-- state, which is a degradation, while an exception here costs the CURRENT
-- process a clean shutdown and leaves the port held. The first is recoverable
-- and the second is the outage.
writeHandover :: IO ()
writeHandover = do
  mf <- readIORef handoverExporter
  mp <- lookupEnv handoverEnvOut
  case (mf, mp) of
    (Just f, Just p) -> do
      r <- try (f >>= BL.writeFile p) :: IO (Either SomeException ())
      case r of
        Left _  -> pure ()
        Right _ -> pure ()
    _ -> pure ()

-- | How to launch one forked server.
data ZygoteSpec = ZygoteSpec
  { zsName    :: String
    -- ^ Label for the child, used in logs and by the caller to track it.
  , zsLogFile :: FilePath
    -- ^ The child's @stdout@ AND @stderr@ are redirected here.
    --
    -- __Not optional.__ A forked child inherits the repl's descriptors, and
    -- the repl's stdout is the channel the session speaks its protocol on --
    -- a server logging a line into it corrupts the session's next verdict.
  , zsEnv     :: [(String, String)]
    -- ^ Environment applied IN THE CHILD, after the fork.
    --
    -- This is the point: @setEnv@ in a session @eval@ persists in the repl and
    -- leaks into every later check, but a child's environment dies with the
    -- child. Port selection belongs here.
  , zsDetach  :: Bool
    -- ^ 'createSession' in the child, so it leaves the repl's process group.
    --
    -- With this 'False' the child is in the session's foreground process
    -- group and stopping the session takes it down too. 'True' lets the server outlive a repl restart, which also means
    -- nothing reaps it but the caller.
  }

-- | A forked server the caller is now responsible for.
data ZygoteChild = ZygoteChild
  { zcPid  :: ProcessID
  , zcName :: String
  , zcLog  :: FilePath
  }

-- | Attached (dies with the session), logging to @\/tmp\/zygote-NAME.log@.
defaultZygoteSpec :: String -> ZygoteSpec
defaultZygoteSpec name = ZygoteSpec
  { zsName    = name
  , zsLogFile = "/tmp/zygote-" ++ name ++ ".log"
  , zsEnv     = []
  , zsDetach  = False
  }

-- | Positional spec, for callers that build this expression as TEXT.
--
-- @ghci-session@ constructs the fork as a string and
-- evaluates it in the repl, where the module is necessarily imported
-- QUALIFIED -- and GHC 9.14 rejects qualified record-update syntax on a
-- field it can also see as a selector:
--
-- @
--     Ambiguous record field \u2018zsLogFile\u2019.
--     It could refer to any of the following:
--       * variable \u2018zsLogFile\u2019, imported qualified from ...
--       * record field \u2018zsLogFile\u2019 of \u2018ZygoteSpec\u2019
-- @
--
-- Arguments are name, log file, child environment, detach.
zygoteSpec :: String -> FilePath -> [(String, String)] -> Bool -> ZygoteSpec
zygoteSpec n l e d = ZygoteSpec
  { zsName = n, zsLogFile = l, zsEnv = e, zsDetach = d }

-- | Fork @act@ into its own process.
--
-- Only the forking thread survives in the child: a lock held by another Haskell thread at the instant of
-- the fork stays held forever there. The session forks from an idle repl, between commands.
--
-- The timer\/IO manager DOES survive -- a @threadDelay@ in the child completes
-- and the child can bind and serve -- because the RTS restarts them in the
-- child after the fork.
zygoteFork :: ZygoteSpec -> IO () -> IO ZygoteChild
zygoteFork spec act = do
  pid <- forkProcess (childMain spec act)
  pure ZygoteChild { zcPid = pid, zcName = zsName spec, zcLog = zsLogFile spec }

childMain :: ZygoteSpec -> IO () -> IO ()
childMain spec act = do
  when (zsDetach spec) (void (try (void createSession) :: IO (Either SomeException ())))
  -- stdin first: a server must never read the repl's protocol channel.
  devnull <- openFd "/dev/null" ReadOnly defaultFileFlags
  _ <- dupTo devnull stdInput
  closeFd devnull
  fd <- openFd (zsLogFile spec) WriteOnly
          defaultFileFlags { creat = Just 0o644, trunc = True }
  _ <- dupTo fd stdOutput
  _ <- dupTo fd stdError
  closeFd fd
  mapM_ (uncurry setEnv) (zsEnv spec)
  -- A forked child was observed IGNORING SIGTERM (it kept serving, and kept
  -- its listening socket, so the replacement could not bind). Without this
  -- handler the only way to stop one is SIGKILL.
  -- Export BEFORE exiting: this is the only moment the old process's state
  -- still exists and the parent has already decided to replace it.
  _ <- installHandler sigTERM
         (Catch (writeHandover >> exitImmediately (ExitFailure 143))) Nothing
  -- The repl's argv is cabal's, and a model @main@ that dispatches on
  -- getArgs would pick a probe flag out of it.
  withArgs [] act

-- | Is the child still running? Reaps it as a side effect if it has exited,
-- which is the only thing that clears the zombie GHCi otherwise leaves.
zygoteAlive :: ZygoteChild -> IO Bool
zygoteAlive c = do
  r <- try (getProcessStatus False True (zcPid c))
         :: IO (Either SomeException (Maybe ProcessStatus))
  pure $ case r of
    Left _          -> False   -- no such process
    Right Nothing   -> True    -- still running, nothing to reap
    Right (Just _)  -> False   -- exited, and now reaped

-- | SIGTERM, wait up to @n@ tenths of a second, then SIGKILL.
--
-- Returns 'True' if the child was gone without needing the kill.
zygoteStop :: ZygoteChild -> Int -> IO Bool
zygoteStop c tenths = do
  _ <- try (signalProcess sigTERM (zcPid c)) :: IO (Either SomeException ())
  gone <- waitGone tenths
  if gone then pure True else do
    _ <- try (signalProcess sigKILL (zcPid c)) :: IO (Either SomeException ())
    _ <- waitGone 20
    pure False
  where
    waitGone 0 = not <$> zygoteAlive c
    waitGone k = do
      alive <- zygoteAlive c
      if not alive then pure True else do
        threadDelayTenth
        waitGone (k - 1)

threadDelayTenth :: IO ()
threadDelayTenth = threadDelay 100000

-- | Drain every exited child. GHCi installs no SIGCHLD handling, so a forked
-- server that exits stays a zombie until someone asks for its status.
zygoteReap :: [ZygoteChild] -> IO [ZygoteChild]
zygoteReap cs = do
  alives <- mapM (\c -> (,) c <$> zygoteAlive c) cs
  pure [ c | (c, True) <- alives ]
