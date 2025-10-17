module Registrar
  ( runApp
  ) where

import Registrar.Prelude

import Registrar.API (runApi)
import Registrar.Database

import Control.Monad.Logger (NoLoggingT (runNoLoggingT))
import Data.ByteString qualified as B
import Data.Kind (Type)
import Data.Text qualified as T
import Database.Persist.Postgresql (createPostgresqlPool)
import Network.Wai.Handler.Warp qualified as WP
import Options.Generic
import Registrar.Bot.State

type Options :: Type -> Type
data Options w = Options
  { port :: !(w ::: Int <?> "Port to listen on" <!> "9060" <#> "p")
  , database :: !(w ::: B.ByteString <?> "Database connection string" <#> "d")
  , databasePoolSize :: !(w ::: Int <?> "Database pool size" <!> "10" <#> "s")
  , migrations :: !(w ::: Bool <?> "Run migrations" <!> "False" <#> "m")
  , datasetFolder :: !(w ::: FilePath <?> "Default dataset folder" <#> "f")
  , botToken :: !(w ::: String <?> "Telegram bot token" <#> "t")
  }
  deriving stock (Generic)

deriving anyclass instance ParseRecord (Options Wrapped)
deriving stock instance Show (Options Unwrapped)

runApp :: IO ()
runApp = do
  (op :: Options Unwrapped) <- unwrapRecord "Registrar application"
  pool <- runNoLoggingT $ createPostgresqlPool op.database op.databasePoolSize
  let ?pool = pool
  migrateDb
  -- importFromDataset op.datasetFolder
  cm <- communityList
  st <- newBotState Settings{botName = "floss bot", botToken = T.pack op.botToken, debugEnabled = True} cm
  WP.run op.port $ runApi st
