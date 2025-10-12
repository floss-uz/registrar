module Registrar
  ( runApp
  ) where

import Registrar.Database

import Control.Monad.Logger (NoLoggingT (runNoLoggingT))
import Data.ByteString qualified as B
import Data.Kind (Type)
import Database.Persist.Postgresql (createPostgresqlPool)
import Options.Generic

type Options :: Type -> Type
data Options w = Options
  { port :: !(w ::: Int <?> "Port to listen on" <!> "9060" <#> "p")
  , database :: !(w ::: B.ByteString <?> "Database connection string" <#> "d")
  , databasePoolSize :: !(w ::: Int <?> "Database pool size" <!> "10" <#> "s")
  , migrations :: !(w ::: Bool <?> "Run migrations" <!> "False" <#> "m")
  , datasetFolder :: !(w ::: FilePath <?> "Default dataset folder" <#> "f")
  }
  deriving stock (Generic)

deriving anyclass instance ParseRecord (Options Wrapped)
deriving stock instance Show (Options Unwrapped)

runApp :: IO ()
runApp = do
  (op :: Options Unwrapped) <- unwrapRecord "Registrar application"
  print op
  pool <- runNoLoggingT $ createPostgresqlPool op.database op.databasePoolSize
  let ?pool = pool
  migrateDb
  importFromDataset op.datasetFolder
