{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module SaveCompany where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import GHC.Generics
import System.IO.Unsafe ( unsafePerformIO )
import System.IO
import System.Directory
import ModelCompany

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
  let companiesList = companyList ++ [giveIdForCompany company (length companyList)]
  B.writeFile "../data/ArquivoTemporario.json" $ encode companiesList
  removeFile jsonFilePath
  renameFile "../data/ArquivoTemporario.json" jsonFilePath

-- Edita as ações da Empresa
editCompanyInfoJSON :: String -> Company -> IO ()
editCompanyInfoJSON jsonFilePath updatedCompany = do
 let companiesList = getCompanyJSON jsonFilePath
 let newCompaniesList = removeCompanyByID (identifier updatedCompany) companiesList ++ [updatedCompany]
 B.writeFile "../data/ArquivoTemporario.json" $ encode newCompaniesList
 removeFile jsonFilePath
 renameFile "../data/ArquivoTemporario.json" jsonFilePath

-- Remove uma empresa pelo ID
removeCompanyJSON :: String -> Int -> IO ()
removeCompanyJSON jsonFilePath identifier = do
 let companiesList = getCompanyJSON jsonFilePath
 let newCompaniesList = removeCompanyByID identifier companiesList
 B.writeFile "../data/ArquivoTemporario.json" $ encode newCompaniesList
 removeFile jsonFilePath
 renameFile "../data/ArquivoTemporario.json" jsonFilePath

-- Atribui novo id ao cliente
giveIdForCompany :: Company -> Int -> Company
giveIdForCompany client newId = client { identifier = newId }
