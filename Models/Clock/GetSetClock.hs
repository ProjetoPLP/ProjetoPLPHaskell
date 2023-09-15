module Models.Clock.GetSetClock where

import qualified Data.ByteString.Lazy as B
import Data.Aeson ( encode, decode )
import System.IO.Unsafe ( unsafePerformIO )
import Models.Clock.ModelClock ( Clock (Clock, minutes) )


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
            let newMinutes = minutes oldClock + increment
                newClock = Clock { minutes = newMinutes }
            B.writeFile "./Data/Clock.json" $ encode newClock
        Nothing -> putStrLn "Erro ao ler o arquivo JSON"


-- Substitui os minutos do relógio
setClock :: Int -> IO ()
setClock increment = do 
    jsonContent <- B.readFile "./Data/Clock.json"
    let maybeClock = decode jsonContent :: Maybe Clock
    case maybeClock of
        Just oldClock -> do
            let newClock = Clock { minutes = increment }
            B.writeFile "./Data/Clock.json" $ encode newClock
        Nothing -> putStrLn "Erro ao ler o arquivo JSON"