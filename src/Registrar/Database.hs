{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Registrar.Database
  ( migrateDb
  , communityList
  , Community (..)
  , CommunityId (..)
  , PoolSql (..)
  , importFromDataset
  ) where

import Database.Persist.Migration (checkMigration, defaultSettings)
import Database.Persist.Migration.Postgres (runMigration)

import Control.Monad.Reader (ReaderT, runReaderT)

import Control.Monad.IO.Class (MonadIO (liftIO))
import Registrar.Database.Migrations

import Data.ByteString.Lazy qualified as B
import Data.Kind (Constraint, Type)
import Data.Pool (Pool, withResource)
import Database.Persist.SqlBackend (SqlBackend)
import GHC.Generics (Generic)

import Data.Aeson hiding (Key)

import Control.Monad (forM_)
import Database.Esqueleto.Experimental hiding (runMigration)
import Database.Persist.TH

type PoolSql :: Constraint
type PoolSql = (?pool :: Pool SqlBackend)

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

share
  [mkPersist sqlSettings{mpsPrefixFields = False}]
  [persistLowerCase|
  Community sql=communities
    established String
    mission String
    chat String Maybe
    manager String Maybe
    github String
    website String Maybe
    deriving Eq 
|]

type Community :: Type
type CommunityId :: Type

deriving stock instance Show Community
deriving stock instance Generic Community
deriving anyclass instance ToJSON Community
deriving anyclass instance FromJSON Community

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
