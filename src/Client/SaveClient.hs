{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Client.SaveClient where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import GHC.Generics
import System.IO.Unsafe ( unsafePerformIO )
import System.IO
import System.Directory
import Client.ModelClient
import Data.Maybe

instance FromJSON Client
instance ToJSON Client
instance FromJSON Asset
instance ToJSON Asset

getClient :: Int -> Client
getClient id = getClientsByID id (getClientJSON "./Data/Clients.json")

-- ====================== GetClient ========================= --
-- Entrada: id:Int / clients:[Client]
-- TipoDeSaida: Client
getClientsByID :: Int -> [Client] -> Client
getClientsByID _ [] = Client (-1) "" 0 0 "" 0 0 0.00 False 19 52 []
getClientsByID identifierS (x:xs)
 | (ident x) == identifierS = x
 | otherwise = getClientsByID identifierS xs

-- ====================== RemoveClient ====================== --
-- Entrada: id:ID / clients:[Client]
-- TipoDeSaida: [Client]
removeClientByID :: Int -> [Client] -> [Client]
removeClientByID _ [] = []
removeClientByID identifierS (x:xs)
 | (ident x) == identifierS = xs
 | otherwise = [x] ++ (removeClientByID identifierS xs)

-- ====================== getAllClients ===================== --
-- Entrada: path:String
-- TipoDeSaida: [Client]
getClientJSON :: String -> [Client]
getClientJSON path = do
 let file = unsafePerformIO (B.readFile path)
 let decodedFile = decode file :: Maybe [Client]
 case decodedFile of
  Nothing -> []
  Just out -> out

-- ====================== SaveClient ======================== --
-- Entrada: path:String / client:Client
-- TipoDeSaida: None
saveClientJSON :: String -> Client -> IO ()
saveClientJSON jsonFilePath client = do
  let clientList = getClientJSON jsonFilePath
  let newID = length clientList + 1
  let clientsList = clientList ++ [giveIdForClient client (newID)]

  textoContents <- readFile "./Sprites/wallet.txt"
  let walletFileName = "./Client/Wallet/wallet" ++ (show newID) ++ ".txt"
  appendFile walletFileName textoContents

  B.writeFile "./Data/ArquivoTemporario.json" $ encode clientsList
  removeFile jsonFilePath
  renameFile "./Data/ArquivoTemporario.json" jsonFilePath

-- ====================== EditCashClient ==================== --
-- Entrada: path:String / client:Client
-- TipoDeSaida: None
editClientJSON :: String -> Client -> IO ()
editClientJSON jsonFilePath updatedClient = do
 let clientsList = getClientJSON jsonFilePath
 let newClientsList = removeClientByID (ident updatedClient) clientsList ++ [updatedClient]
 B.writeFile "./Data/ArquivoTemporario.json" $ encode newClientsList
 removeFile jsonFilePath
 renameFile "./Data/ArquivoTemporario.json" jsonFilePath

-- ====================== RemoveClient ====================== --
-- Entrada: path:String / id:Int
-- TipoDeSaida: None
removeClientJSON :: String -> Int -> IO ()
removeClientJSON jsonFilePath ident = do
 let clientsList = getClientJSON jsonFilePath
 let newClientsList = removeClientByID ident clientsList
 B.writeFile "./Data/ArquivoTemporario.json" $ encode newClientsList
 removeFile jsonFilePath
 renameFile "./Data/ArquivoTemporario.json" jsonFilePath

-- ====================== ExistClient ======================= --
-- Entrada: email:String
-- TipoDeSaida: Bool
existClientByEmail :: String -> Bool
existClientByEmail email = verifyExistEmailClient email (getClientJSON "./Data/Clients.json")

-- ====================== ExistClient ======================= --
-- Entrada: email:String / client:Client
-- TipoDeSaida: Bool
verifyExistEmailClient :: String -> [Client] -> Bool
verifyExistEmailClient _ [] = False
verifyExistEmailClient emailClient (head:tail) = 
  if emailClient == (email head) then True
  else verifyExistEmailClient emailClient tail

-- ====================== GetClient ========================= --
-- Entrada: client:Client / NewID:Int
-- TipoDeSaida: Client
giveIdForClient :: Client -> Int -> Client
giveIdForClient client newId = client { ident = newId }

-- ====================== SaveClient ======================== --
-- Entrada: client:Client
-- TipoDeSaida: None
saveLoginJSON :: Client -> IO ()
saveLoginJSON client = do
  B.writeFile "./Data/ArquivoTemporario.json" $ encode client
  removeFile "./Data/Login.json"
  renameFile "./Data/ArquivoTemporario.json" "./Data/Login.json"

-- ====================== ExistClientLogged ================== --
-- Entrada: None
-- TipoDeSaida: Bool
hasLoginDataInJSON :: IO Bool
hasLoginDataInJSON = do
  jsonContent <- B.readFile "./Data/Login.json"
  let result = decode jsonContent :: Maybe Client
  return $ isJust result

-- ====================== LogoutClient ======================= --
-- Entrada: None
-- TipoDeSaida: None
logoutClient :: IO()
logoutClient = do
    withFile "./Data/Login.json" WriteMode $ \handle -> do
        let emptyContent = B.empty
        B.hPut handle emptyContent

-- ====================== GetClientDefault ================== --
-- Entrada: client:Client
-- TipoDeSaida: Client
defaultClient :: Client
defaultClient = Client (-1) "" 0 0 "" 0 0 0.00 False 19 52 []

-- ====================== ReadClient ======================== --
-- Entrada: Path:String
-- TipoDeSaida: Client
readClientFromFile :: FilePath -> IO (Either String Client)
readClientFromFile filePath = eitherDecodeFileStrict filePath

-- ====================== GetLogin ========================== --
-- Entrada: None
-- TipoDeSaida: Client / Nothing
getClientLogado :: IO (Maybe Client)
getClientLogado = do
    result <- readClientFromFile "./Data/Login.json"
    return (either (const Nothing) Just result)
