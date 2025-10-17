module Registrar.Database.Migrations
  ( allMigrations
  ) where

import Database.Persist.Migration

createUser :: Operation
createUser =
  CreateTable
    { name = "users"
    , schema =
        [ Column "id" SqlInt32 [NotNull, AutoIncrement]
        , Column "name" SqlString [NotNull]
        , Column "username" SqlString [NotNull]
        ]
    , constraints =
        [ PrimaryKey ["id"]
        ]
    }

createCommunities :: Operation
createCommunities =
  CreateTable
    { name = "communities"
    , schema =
        [ Column "id" SqlInt32 [NotNull, AutoIncrement]
        , Column "name" SqlString [NotNull]
        , Column "established" SqlString [NotNull]
        , Column "mission" SqlString [NotNull]
        , Column "chat" SqlString []
        , Column "manager" SqlString []
        , Column "github" SqlString [NotNull]
        , Column "website" SqlString []
        ]
    , constraints =
        [ PrimaryKey ["id"]
        ]
    }

allMigrations :: Migration
allMigrations =
  [ 0 ~> 1 := [createUser]
  , 1 ~> 2 := [createCommunities]
  ]
