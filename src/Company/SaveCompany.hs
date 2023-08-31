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

getCompaniesByID :: Int -> [Company] -> Company
getCompaniesByID _ [] = Company (-1) "" 0 0 "" "" 0 0.00
getCompaniesByID identifierS (x:xs)
 | (identifier x) == identifierS = x
 | otherwise = getCompaniesByID identifierS xs

removeCompanyByID :: Int -> [Company] -> [Company]
removeCompanyByID _ [] = []
removeCompanyByID identifierS (x:xs)
 | (identifier x) == identifierS = xs
 | otherwise = [x] ++ (removeCompanyByID identifierS xs)

getCompanyJSON :: String -> [Company]
getCompanyJSON path = do
 let file = unsafePerformIO (B.readFile path)
 let decodedFile = decode file :: Maybe [Company]
 case decodedFile of
  Nothing -> []
  Just out -> out

saveCompanyJSON :: String -> Company -> IO ()
saveCompanyJSON jsonFilePath company = do
  let companyList = getCompanyJSON jsonFilePath
  let companiesList = companyList ++ [company]
  B.writeFile "./ArquivoTemporario.json" $ encode companiesList
  removeFile jsonFilePath
  renameFile "./ArquivoTemporario.json" jsonFilePath


editCompanyInfoJSON :: String -> Company -> IO ()
editCompanyInfoJSON jsonFilePath updatedCompany = do
 let companiesList = getCompanyJSON jsonFilePath
 let newCompaniesList = removeCompanyByID (identifier updatedCompany) companiesList ++ [updatedCompany]
 B.writeFile "./ArquivoTemporario.json" $ encode newCompaniesList
 removeFile jsonFilePath
 renameFile "./ArquivoTemporario.json" jsonFilePath

removeCompanyJSON :: String -> Int -> IO ()
removeCompanyJSON jsonFilePath identifier = do
 let companiesList = getCompanyJSON jsonFilePath
 let newCompaniesList = removeCompanyByID identifier companiesList
 B.writeFile "./ArquivoTemporario.json" $ encode newCompaniesList
 removeFile jsonFilePath
 renameFile "./ArquivoTemporario.json" jsonFilePath
