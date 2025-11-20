module Registrar.Database.Community (getAll) where

import Database.Esqueleto.Experimental hiding (runMigration)
import Registrar.Database
import Registrar.Database.Types

import Registrar.Types qualified as RT

getAll :: (PoolSql) => IO [RT.Community]
getAll = map entityToType <$> withPool (select $ from table)

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
