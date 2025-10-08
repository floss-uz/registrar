module Registrar.Database
  ( migrateDb
  ) where

import Database.Persist.Migration (checkMigration, defaultSettings)
import Database.Persist.Migration.Postgres (runMigration)

import Control.Monad.Reader (ReaderT, runReaderT)

import Control.Monad.IO.Class (MonadIO (liftIO))
import Registrar.Database.Migrations

import Data.Kind (Constraint, Type)
import Data.Pool (Pool, withResource)
import Database.Persist.SqlBackend (SqlBackend)
import GHC.Generics

-- import Database.Esqueleto.Experimental

type PoolSql :: Constraint
type PoolSql = (?pool :: Pool SqlBackend)

withPool :: (?pool :: Pool s) => ReaderT s IO r -> IO r
withPool = withResource ?pool . runReaderT

migrateDb :: (PoolSql) => IO ()
migrateDb =
  withPool $ runMigration defaultSettings allMigrations
