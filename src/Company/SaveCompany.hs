{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Company.SaveCompany where

import Data.Aeson
import Data.List (sort)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import GHC.Generics
import System.IO.Unsafe ( unsafePerformIO )
import System.IO
import System.Directory
import Company.ModelCompany

instance FromJSON Company
instance ToJSON Company

getCompany :: Int -> Company
getCompany id = getCompaniesByID id (getCompanyJSON "./Data/Companies.json")

-- Pega uma empresa pelo ID
getCompaniesByID :: Int -> [Company] -> Company
getCompaniesByID _ [] = Company (-1) "" 0 "" "" "" "" 0.00 " " 0.00 0.00 0.00 0 0
getCompaniesByID identifierS (x:xs)
 | (ident x) == identifierS = x
 | otherwise = getCompaniesByID identifierS xs

-- Remove uma empresa pelo ID
removeCompanyByID :: Int -> [Company] -> [Company]
removeCompanyByID _ [] = []
removeCompanyByID identifierS (x:xs)
 | (ident x) == identifierS = xs
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
  let newID = identifySequenceBreak (getIds $ getCompanyJSON "./Data/Companies.json")
  let companiesList = companyList ++ [giveIdForCompany company (newID)]
  
  textoContents <- readFile "./Sprites/HomeBroker/homebroker_base.txt"
  let walletFileName = "./Company/HomeBroker/homebroker" ++ (show newID) ++ ".txt"
  appendFile walletFileName textoContents

  B.writeFile "./Data/ArquivoTemporario.json" $ encode companiesList
  removeFile jsonFilePath
  renameFile "./Data/ArquivoTemporario.json" jsonFilePath


-- Identifica uma quebra em uma sequência numérica, retornando o primeiro valor ausente
identifySequenceBreak :: [Int] -> Int
identifySequenceBreak [] = 1
identifySequenceBreak (x:xs)
    | not (null xs) && x + 1 /= head xs = x + 1
    | null xs = x + 1
    | otherwise = identifySequenceBreak xs


-- Retorna uma lista com todos os IDs das empresas
getIds :: [Company] -> [Int]
getIds companies = sort [ident x | x <- companies]


-- Edita as ações da Empresa
editCompanyJSON :: String -> Company -> IO ()
editCompanyJSON jsonFilePath updatedCompany = do
 let companiesList = getCompanyJSON jsonFilePath
 let newCompaniesList = removeCompanyByID (ident updatedCompany) companiesList ++ [updatedCompany]
 B.writeFile "./Data/ArquivoTemporario.json" $ encode newCompaniesList
 removeFile jsonFilePath
 renameFile "./Data/ArquivoTemporario.json" jsonFilePath

-- Remove uma empresa pelo ID
removeCompany :: Int -> String -> IO ()
removeCompany idComp jsonFilePath = do
 let companiesList = getCompanyJSON jsonFilePath
 let newCompaniesList = removeCompanyByID idComp companiesList
 B.writeFile "./Data/ArquivoTemporario.json" $ encode newCompaniesList
 removeFile jsonFilePath
 renameFile "./Data/ArquivoTemporario.json" jsonFilePath
 let filePathToDelete = "./Company/HomeBroker/homebroker" ++ show idComp ++ ".txt"
 removeFile filePathToDelete


-- Verifica a existencia do cliente pelo email
existCompanyByName :: String -> Bool
existCompanyByName name = verifyExistNameCompany name (getCompanyJSON "./Data/Clients.json")

verifyExistNameCompany :: String -> [Company] -> Bool
verifyExistNameCompany _ [] = False
verifyExistNameCompany nameCompany (head:tail) = 
  if nameCompany == (name head) then True
  else verifyExistNameCompany nameCompany tail

-- Atribui novo id ao cliente
giveIdForCompany :: Company -> Int -> Company
giveIdForCompany client newId = client { ident = newId }
