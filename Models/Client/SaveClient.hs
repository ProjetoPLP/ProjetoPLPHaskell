{-# LANGUAGE OverloadedStrings #-}

module Models.Client.SaveClient where

import Data.Aeson ( FromJSON, ToJSON, encode, decode )
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import System.IO.Unsafe ( unsafePerformIO )
import System.Directory ( removeFile, renameFile )
import Models.Client.ModelClient ( Asset, Client(Client, ident, email) )

instance FromJSON Client
instance ToJSON Client
instance FromJSON Asset
instance ToJSON Asset


getClient :: Int -> Client
getClient id = getClientsByID id (getClientJSON "./Data/Clients.json")


getClientsByID :: Int -> [Client] -> Client
getClientsByID _ [] = Client (-1) "" "" "" "" "" 0 0 " " False 19 52 []
getClientsByID identifierS (x:xs)
    | ident x == identifierS = x
    | otherwise = getClientsByID identifierS xs


removeClientByID :: Int -> [Client] -> [Client]
removeClientByID _ [] = []
removeClientByID identifierS (x:xs)
    | ident x == identifierS = xs
    | otherwise = x : removeClientByID identifierS xs


getClientJSON :: String -> [Client]
getClientJSON path = do
    let file = unsafePerformIO (B.readFile path)
        decodedFile = decode file :: Maybe [Client]
    case decodedFile of
        Nothing -> []
        Just out -> out


saveClientJSON :: String -> Client -> IO ()
saveClientJSON jsonFilePath client = do
    let clientList = getClientJSON jsonFilePath
        newID = length clientList + 1
        clientsList = clientList ++ [giveIdForClient client newID]

    textoContents <- readFile "./Sprites/Wallet/wallet_base.txt"
    let walletFileName = "./Models/Client/Wallets/wallet" ++ show newID ++ ".txt"
    appendFile walletFileName textoContents

    B.writeFile "./Data/ArquivoTemporario.json" $ encode clientsList
    removeFile jsonFilePath
    renameFile "./Data/ArquivoTemporario.json" jsonFilePath


editClientJSON :: String -> Client -> IO ()
editClientJSON jsonFilePath updatedClient = do
    let clientsList = getClientJSON jsonFilePath
        newClientsList = removeClientByID (ident updatedClient) clientsList ++ [updatedClient]
    B.writeFile "./Data/ArquivoTemporario.json" $ encode newClientsList
    removeFile jsonFilePath
    renameFile "./Data/ArquivoTemporario.json" jsonFilePath


removeClientJSON :: String -> Int -> IO ()
removeClientJSON jsonFilePath ident = do
    let clientsList = getClientJSON jsonFilePath
        newClientsList = removeClientByID ident clientsList
    B.writeFile "./Data/ArquivoTemporario.json" $ encode newClientsList
    removeFile jsonFilePath
    renameFile "./Data/ArquivoTemporario.json" jsonFilePath


existClientByEmail :: String -> Bool
existClientByEmail email = verifyExistEmailClient email (getClientJSON "./Data/Clients.json")


verifyExistEmailClient :: String -> [Client] -> Bool
verifyExistEmailClient _ [] = False
verifyExistEmailClient emailClient (head:tail)
    | emailClient == email head = True
    | otherwise = verifyExistEmailClient emailClient tail


giveIdForClient :: Client -> Int -> Client
giveIdForClient client newId = client { ident = newId }