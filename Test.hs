module Main (main) where

import Control.Concurrent
import Control.Monad

import System.SelfRestart (selfRestart, forkSelfRestartExePollWithAction)

main :: IO ()
main = do

  -- Spawn thread to restart when executable changes
  forkSelfRestartExePollWithAction 1.0 $
    putStrLn "executable changed, restarting"

  forM_ [1..4] $ \x -> do
    print x
    threadDelay 1000000

  -- Restart when the loop is over
  putStrLn "counter done, selfRestart"
  selfRestart
