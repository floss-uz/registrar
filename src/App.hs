module App
  ( runApp
  ) where

import CLI

runApp :: IO ()
runApp = do
  runCli
  print "running"
