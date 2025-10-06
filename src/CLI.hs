module CLI (runCli) where

import Options.Generic

data CommandOptions
  = Migrations {run :: Bool <?> "Run database migrations"} -- app migrations --run
  | Run {config :: Text <?> "Configuration toml file"} -- app run --config config.file
  deriving (Generic, Show)

instance ParseRecord CommandOptions

runCli :: IO ()
runCli = do
  x <- getRecord "Welcome user !"
  print (x :: CommandOptions)
