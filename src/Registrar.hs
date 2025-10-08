module Registrar
  ( runApp
  ) where

import Data.ByteString qualified as B
import Data.Kind (Type)
import Options.Generic

type Options :: Type -> Type
data Options w = Options
  { port :: !(w ::: Int <?> "Port to listen on" <!> "9060" <#> "p")
  , database :: !(w ::: B.ByteString <?> "Database connection string" <#> "u")
  , databasePoolSize :: !(w ::: Int <?> "Database pool size" <!> "10" <#> "s")
  , migrations :: !(w ::: Bool <?> "Run migrations" <!> "False" <#> "m")
  }
  deriving stock (Generic)

deriving anyclass instance ParseRecord (Options Wrapped)
deriving stock instance Show (Options Unwrapped)

runApp :: IO ()
runApp = do
  (op :: Options Unwrapped) <- unwrapRecord ""
  print op
