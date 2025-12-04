module Registrar.Types.Community where

import Data.Int (Int64)
import Data.OpenApi (ToSchema)
import Registrar.Prelude

data Community = MkCommunity
  { id :: !Int64
  , name :: !Text
  , established :: !Text
  , mission :: !Text
  , chat :: !Text
  , manager :: !(Maybe Text)
  , github :: !Text
  , website :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

deriving anyclass instance ToJSON Community
deriving anyclass instance ToSchema Community
