{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Client.SaveCompany where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import GHC.Generics
import System.IO.Unsafe ( unsafePerformIO )
import System.IO
import System.Directory
import Client.ModelCompany

instance FromJSON Company
instance ToJSON Company

-- Pega uma empresa pelo ID
getCompaniesByID :: Int -> [Company] -> Company
getCompaniesByID _ [] = Company (-1) "" 0 0 "" "" 0 0.00
getCompaniesByID identifierS (x:xs)
 | (identifier x) == identifierS = x
 | otherwise = getCompaniesByID identifierS xs

-- Remove uma empresa pelo ID
removeCompanyByID :: Int -> [Company] -> [Company]
removeCompanyByID _ [] = []
removeCompanyByID identifierS (x:xs)
 | (identifier x) == identifierS = xs
 | otherwise = [x] ++ (removeCompanyByID identifierS xs)

-- Pega todas as empresas salvas
getCompanyJSON :: String -> [Company]
getCompanyJSON path = do
 let file = unsafePerformIO (B.readFile path)
 let decodedFile = decode file :: Maybe [Company]
 case decodedFile of
  Nothing -> []
  Just out -> out

-- Salva uma Empresa
saveCompanyJSON :: String -> Company -> IO ()
saveCompanyJSON jsonFilePath company = do
  let companyList = getCompanyJSON jsonFilePath
  let newID = length companyList + 1
  let companiesList = companyList ++ [giveIdForCompany company (newID)]

  textoContents <- readFile "../Sprites/homebroker.txt"
  let walletFileName = "./HomeBroker/homebroker" ++ (show newID) ++ ".txt"
  appendFile walletFileName textoContents

  B.writeFile "../Data/ArquivoTemporario.json" $ encode companiesList
  removeFile jsonFilePath
  renameFile "../Data/ArquivoTemporario.json" jsonFilePath

-- Edita as ações da Empresa
editCompanyJSON :: String -> Company -> IO ()
editCompanyJSON jsonFilePath updatedCompany = do
 let companiesList = getCompanyJSON jsonFilePath
 let newCompaniesList = removeCompanyByID (identifier updatedCompany) companiesList ++ [updatedCompany]
 B.writeFile "../Data/ArquivoTemporario.json" $ encode newCompaniesList
 removeFile jsonFilePath
 renameFile "../Data/ArquivoTemporario.json" jsonFilePath

-- Remove uma empresa pelo ID
removeCompanyJSON :: String -> Int -> IO ()
removeCompanyJSON jsonFilePath identifier = do
 let companiesList = getCompanyJSON jsonFilePath
 let newCompaniesList = removeCompanyByID identifier companiesList
 B.writeFile "../Data/ArquivoTemporario.json" $ encode newCompaniesList
 removeFile jsonFilePath
 renameFile "../Data/ArquivoTemporario.json" jsonFilePath

-- Verifica a existencia do cliente pelo email
existCompanyByName :: String -> Bool
existCompanyByName name = verifyExistNameCompany name (getCompanyJSON "../Data/Clients.json")

verifyExistNameCompany :: String -> [Company] -> Bool
verifyExistNameCompany _ [] = False
verifyExistNameCompany nameCompany (head:tail) = 
  if nameCompany == (name head) then True
  else verifyExistNameCompany nameCompany tail

-- Atribui novo id ao cliente
giveIdForCompany :: Company -> Int -> Company
giveIdForCompany client newId = client { identifier = newId }
