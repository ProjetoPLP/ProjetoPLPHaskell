module Models.Clock.ModelClock where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

newtype Clock = Clock
  { 
    minutes :: Int
  } deriving (Show, Generic)

instance ToJSON Clock
instance FromJSON Clock