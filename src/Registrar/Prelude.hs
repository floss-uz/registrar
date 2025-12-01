module Registrar.Prelude
  ( Text
  , Constraint
  , Type
  , Generic
  , FromJSON
  , ToJSON
  , timeExpired
  , currentTimeSec
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Kind (Constraint, Type)
import Data.Text (Text)
import Data.Time (UTCTime, diffUTCTime)
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)

timeExpired :: UTCTime -> UTCTime -> Int -> Bool
timeExpired cTime tTime expInterval =
  let expTime = secondsToNominalDiffTime (fromIntegral expInterval)
   in diffUTCTime cTime tTime >= expTime

currentTimeSec :: IO Int
currentTimeSec = round <$> getPOSIXTime
