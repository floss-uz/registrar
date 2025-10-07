module Registrar
  ( runApp
  ) where

import Registrar.Database

import Control.Monad.Logger (NoLoggingT (runNoLoggingT))
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Database.Persist.Postgresql (createPostgresqlPool)

dbCreds :: (B.ByteString, Int)
dbCreds = (url, 10)
 where
  url = BC.pack "postgresql://localhost/jasliq?user=postgres&password=postgres"

runApp :: IO ()
runApp = do
  pool <- runNoLoggingT $ createPostgresqlPool (fst dbCreds) (snd dbCreds)
  let ?pool = pool
  migrateDb

-- print "running"
