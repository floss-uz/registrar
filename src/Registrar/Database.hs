{-# LANGUAGE RequiredTypeArguments #-}

module Registrar.Database
  ( migrateDb
  , communityList
  , importFromDataset
  ) where

import Registrar.Prelude (FromJSON)
import Registrar.Types (Community, PoolSql)

import Database.Persist.Migration (defaultSettings)
import Database.Persist.Migration.Postgres (runMigration)
import Registrar.Database.Migrations (allMigrations)

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as B
import Data.Pool (Pool, withResource)
import Database.Esqueleto.Experimental hiding (runMigration)
import Database.Persist.SqlBackend (SqlBackend)

-- | Read json file and insert to db json structure must satisfied to entity type.
createDatasetFromFile
  :: forall a
  ->(FromJSON a, PersistEntity a, PersistEntityBackend a ~ SqlBackend, SafeToInsert a)
  => FilePath
  -> SqlPersistT IO ()
createDatasetFromFile tyA filePath = do
  bytes <- liftIO $ B.readFile filePath
  case eitherDecode @[tyA] bytes of
    Left err -> liftIO $ pure ()
    Right (records :: [tyA]) -> forM_ records insert_

-- | Connection pool using for sql database operations
withPool :: (?pool :: Pool s) => ReaderT s IO r -> IO r
withPool = withResource ?pool . runReaderT

migrateDb :: (PoolSql) => IO ()
migrateDb =
  withPool $ runMigration defaultSettings allMigrations

importFromDataset :: (?pool :: Pool SqlBackend) => FilePath -> IO ()
importFromDataset bp =
  runSqlPool
    ( do
        createDatasetFromFile (type Community) (bp <> "/communities.json")
    )
    ?pool

communityList :: (PoolSql) => IO [Community]
communityList = map entityVal <$> withPool (select $ from table)
