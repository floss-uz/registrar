module Registrar.Prelude
  ( Text
  , Constraint
  , Type
  , Generic
  , FromJSON
  , ToJSON
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.Kind (Type, Constraint)
