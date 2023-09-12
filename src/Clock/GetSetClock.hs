module Clock.GetSetClock where

import Data.Aeson ( FromJSON, ToJSON, encode, decode )
import GHC.Generics ( Generic )
import qualified Data.ByteString.Lazy as B
import System.IO.Unsafe (unsafePerformIO)

data Clock = Clock
  { 
    minutes :: Int
  } deriving (Show, Generic)

instance ToJSON Clock
instance FromJSON Clock


-- Retorna os minutos totais do relógio no arquivo Clock.json
getClock :: FilePath -> Int 
getClock jsonFilePath = do
    let file = unsafePerformIO (B.readFile jsonFilePath)
    let decodedFile = decode file :: Maybe Clock
    case decodedFile of
        Just clock -> minutes clock
        Nothing -> -1


-- Soma ao relógio novos minutos
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