module Clock.GetSetClock where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import System.IO.Unsafe (unsafePerformIO)

data Clock = Clock
  { minutes :: Int
  } deriving (Show, Generic)

instance ToJSON Clock
instance FromJSON Clock


getClock :: Int 
getClock = do
    let file = unsafePerformIO (B.readFile "./Data/Clock.json")
    let decodedFile = decode file :: Maybe Clock
    case decodedFile of
        Just clock -> minutes clock
        Nothing -> -1


addClock :: Int -> IO ()
addClock increment = do 
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


saveClockToFile :: Clock -> IO ()
saveClockToFile clock = do
    B.writeFile "./Data/Clock.json" $ encode clock