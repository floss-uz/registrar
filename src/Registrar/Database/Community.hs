module Registrar.Database.Community (getAll, getOne) where

import Database.Esqueleto.Experimental hiding (runMigration)
import Registrar.Database
import Registrar.Database.Types

import Data.Maybe (listToMaybe)
import Registrar.Prelude
import Registrar.Types qualified as RT

getAll :: (PoolSql) => IO [RT.Community]
getAll = entityToType <$$> withPool (select $ from table)

getOne :: (PoolSql) => Text -> IO (Maybe RT.Community)
getOne communityName = do
  results <- withPool $ select $ do
    community <- from $ table @Community
    where_ (community.name `like` val communityName)
    pure community
  pure $ fmap entityToType (listToMaybe results)

entityToType :: Entity Community -> RT.Community
entityToType (Entity k v) =
  RT.MkCommunity
    { id = fromSqlKey $ k
    , name = v.name
    , established = v.established
    , mission = v.mission
    , chat = v.chat
    , manager = v.manager
    , website = v.website
    , github = v.github
    }
