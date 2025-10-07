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

allMigrations :: Migration
allMigrations =
  [ 0 ~> 1 := [createUser]
  ]
