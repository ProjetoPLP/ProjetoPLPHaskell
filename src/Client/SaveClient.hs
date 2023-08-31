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

getClientsByID :: Int -> [Client] -> Client
getClientsByID _ [] = Client (-1) "" 0 0 "" 0 0
getClientsByID identifierS (x:xs)
 | (identifier x) == identifierS = x
 | otherwise = getClientsByID identifierS xs

removeClientByID :: Int -> [Client] -> [Client]
removeClientByID _ [] = []
removeClientByID identifierS (x:xs)
 | (identifier x) == identifierS = xs
 | otherwise = [x] ++ (removeClientByID identifierS xs)

getClientJSON :: String -> [Client]
getClientJSON path = do
 let file = unsafePerformIO (B.readFile path)
 let decodedFile = decode file :: Maybe [Client]
 case decodedFile of
  Nothing -> []
  Just out -> out

saveClientJSON :: String -> Client -> IO ()
saveClientJSON jsonFilePath client = do
  let clientList = getClientJSON jsonFilePath
  let clientsList = clientList ++ [client]
  B.writeFile "./ArquivoTemporario.json" $ encode clientsList
  removeFile jsonFilePath
  renameFile "./ArquivoTemporario.json" jsonFilePath

editClientInfoJSON :: String -> Client -> IO ()
editClientInfoJSON jsonFilePath updatedClient = do
 let clientsList = getClientJSON jsonFilePath
 let newClientsList = removeClientByID (identifier updatedClient) clientsList ++ [updatedClient]
 B.writeFile "../ArquivoTemporario.json" $ encode newClientsList
 removeFile jsonFilePath
 renameFile "../ArquivoTemporario.json" jsonFilePath

removeClientJSON :: String -> Int -> IO ()
removeClientJSON jsonFilePath identifier = do
 let clientsList = getClientJSON jsonFilePath
 let newClientsList = removeClientByID identifier clientsList
 B.writeFile "./ArquivoTemporario.json" $ encode newClientsList
 removeFile jsonFilePath
 renameFile "./ArquivoTemporario.json" jsonFilePath
