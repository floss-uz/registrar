module Registrar.Prelude
  ( Text
  , Constraint
  , Type
  , Generic
  , MonadIO
  , FromJSON
  , ToJSON
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Control.Monad.IO.Class (MonadIO)
import Data.Kind (Type, Constraint)
