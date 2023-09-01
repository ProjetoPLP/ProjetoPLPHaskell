{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module SaveClient where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import GHC.Generics
import System.IO.Unsafe ( unsafePerformIO )
import System.IO
import System.Directory
import ModelClient

instance FromJSON Client
instance ToJSON Client

-- Pega cliente pelo ID (identifier)
getClientsByID :: Int -> [Client] -> Client
getClientsByID _ [] = Client (-1) "" 0 0 "" 0 0
getClientsByID identifierS (x:xs)
 | (identifier x) == identifierS = x
 | otherwise = getClientsByID identifierS xs

-- Remove cliente pelo ID (identifier)
removeClientByID :: Int -> [Client] -> [Client]
removeClientByID _ [] = []
removeClientByID identifierS (x:xs)
 | (identifier x) == identifierS = xs
 | otherwise = [x] ++ (removeClientByID identifierS xs)

-- Pega todos os clientes salvos em JSON
getClientJSON :: String -> [Client]
getClientJSON path = do
 let file = unsafePerformIO (B.readFile path)
 let decodedFile = decode file :: Maybe [Client]
 case decodedFile of
  Nothing -> []
  Just out -> out

-- Salva um cliente em JSON
saveClientJSON :: String -> Client -> IO ()
saveClientJSON jsonFilePath client = do
  let clientList = getClientJSON jsonFilePath
  let newID = length clientList + 1
  let clientsList = clientList ++ [giveIdForClient client (newID)]
  let walletFileName = "./Wallet/wallet" ++ (show newID) ++ ".txt"
  arquivo <- openFile walletFileName WriteMode
  hClose arquivo
  B.writeFile "../Data/ArquivoTemporario.json" $ encode clientsList
  removeFile jsonFilePath
  renameFile "../Data/ArquivoTemporario.json" jsonFilePath

-- Incrementa o cash do cliente
editClientInfoJSON :: String -> Client -> IO ()
editClientInfoJSON jsonFilePath updatedClient = do
 let clientsList = getClientJSON jsonFilePath
 let newClientsList = removeClientByID (identifier updatedClient) clientsList ++ [updatedClient]
 B.writeFile "../Data/ArquivoTemporario.json" $ encode newClientsList
 removeFile jsonFilePath
 renameFile "../Data/ArquivoTemporario.json" jsonFilePath

-- Remove um cliente pelo ID (identifier)
removeClientJSON :: String -> Int -> IO ()
removeClientJSON jsonFilePath identifier = do
 let clientsList = getClientJSON jsonFilePath
 let newClientsList = removeClientByID identifier clientsList
 B.writeFile "../Data/ArquivoTemporario.json" $ encode newClientsList
 removeFile jsonFilePath
 renameFile "../Data/ArquivoTemporario.json" jsonFilePath

-- Verifica a existencia do cliente pelo email
existClientByEmail :: String -> Bool
existClientByEmail email = verifyExistEmailClient email (getClientJSON "../Data/Clients.json")

verifyExistEmailClient :: String -> [Client] -> Bool
verifyExistEmailClient _ [] = False
verifyExistEmailClient emailClient (head:tail) = 
  if emailClient == (email head) then True
  else verifyExistEmailClient emailClient tail

-- Atribui novo id ao cliente
giveIdForClient :: Client -> Int -> Client
giveIdForClient client newId = client { identifier = newId }
