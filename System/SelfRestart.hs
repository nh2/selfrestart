-- | Allows restarting the currently running executable.
--
-- Works with binaries and `runhaskell`/`runghc`. In `ghci` all functions are no-ops.
module System.SelfRestart
  ( -- $usewithcare
    selfRestart
  , forkSelfRestartExePoll
  , forkSelfRestartExePollWithAction
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import System.Posix.Process (executeFile)
import System.Directory (getModificationTime)
import System.Environment.Executable (getExecutablePath, getScriptPath, ScriptPath (..))
import GHC.Environment (getFullArgs)

-- $usewithcare
-- USE WITH CARE. This module uses `getScriptPath` internally, which is /experimental/.
-- Please review it.

-- | Restart the currently running executable.
selfRestart :: IO ()
selfRestart = do
  realExePath <- getExecutablePath
  scriptPath  <- getScriptPath
  fullArgs    <- getFullArgs

  case scriptPath of
    Executable p -> restart p           fullArgs
    RunGHC _     -> restart realExePath fullArgs
    Interactive  -> return ()

  where
    restart exePath args = executeFile exePath False args Nothing


-- | Forks a thread to check poll the own binary's modification time every,
-- and restarts, and restarts it when it gets changed.
--
-- Waits the given number of seconds between checks.
forkSelfRestartExePoll :: Double -> IO (Maybe ThreadId)
forkSelfRestartExePoll pollSeconds = forkSelfRestartExePollWithAction pollSeconds (return ())


-- | Like `forkSelfRestartExePoll`, but you can run an action just before the restart
-- happens.
forkSelfRestartExePollWithAction :: Double -> IO () -> IO (Maybe ThreadId)
forkSelfRestartExePollWithAction pollSeconds shutdownAction = do
  when (pollTimeoutUs < 0) $ error $ "forkSelfRestartExePollWithAction: Bad poll timeout: " ++ show pollSeconds

  realExePath <- getExecutablePath
  scriptPath  <- getScriptPath
  fullArgs    <- getFullArgs

  case scriptPath of
    Executable p -> Just <$> spawn p p           fullArgs
    RunGHC p     -> Just <$> spawn p realExePath fullArgs
    Interactive  -> return Nothing

  where
    pollTimeoutUs = floor $ pollSeconds * 1000000

    spawn watchPath exePath args = do
      mtimeStarted <- getModificationTime watchPath
      forkIO $
        forever $ do
          threadDelay pollTimeoutUs
          mtime <- getModificationTime watchPath
          when (mtime > mtimeStarted) $ do
            shutdownAction
            executeFile exePath False args Nothing
