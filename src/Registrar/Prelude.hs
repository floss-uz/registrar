module Registrar.Prelude
  ( Text
  , Constraint
  , Type
  , Generic
  , FromJSON
  , ToJSON
  , (<$$>)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Kind (Constraint, Type)
import Data.Text (Text)
import GHC.Generics (Generic)

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

infix 4 <$$>
