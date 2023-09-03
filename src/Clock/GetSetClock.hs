{-# LANGUAGE DeriveGeneric #-}
module Clock.GetSetClock where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B

data Clock = Clock
  { minutes :: Int
  } deriving (Show, Generic)

instance ToJSON Clock
instance FromJSON Clock

readClock :: IO Int
readClock = do
    fileContent <- B.readFile "./Data/Clock.json"
    let maybeClock = decode fileContent :: Maybe Clock
    case maybeClock of
        Just clock -> return (minutes clock)
        Nothing    -> return (-1)

saveClockToFile :: Clock -> IO ()
saveClockToFile clock = do
    B.writeFile "./Data/Clock.json" $ encode clock

setClock :: Int -> IO ()
setClock increment = do 
    jsonContent <- B.readFile "./Data/Clock.json"
    let maybeClock = decode jsonContent :: Maybe Clock
    case maybeClock of
        Just oldClock -> do
            let oldMinutes = minutes oldClock
            let newMinutes = oldMinutes + increment
            if newMinutes >= 720 then do
                let newClock = Clock { minutes = 420 }
                saveClockToFile newClock
            else do 
                let newClock = Clock { minutes = newMinutes }
                saveClockToFile newClock
        Nothing -> putStrLn "Erro ao ler o arquivo JSON"
