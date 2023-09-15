{-# LANGUAGE OverloadedStrings #-}

module Models.Company.SaveCompany where

import System.IO.Unsafe ( unsafePerformIO )
import System.Directory ( removeFile, renameFile )
import Data.Aeson ( FromJSON, ToJSON, encode, decode )
import Data.List ( sort )
import qualified Data.ByteString.Lazy as B
import Models.Company.ModelCompany ( Company(Company, ident, name) )

instance FromJSON Company
instance ToJSON Company


getCompany :: Int -> Company
getCompany id = getCompaniesByID id (getCompanyJSON "./Data/Companies.json")


-- Pega uma empresa pelo ID
getCompaniesByID :: Int -> [Company] -> Company
getCompaniesByID _ [] = Company (-1) "" "" "" "" "" "" 0.00 " " 0.00 0.00 0.00 0 0
getCompaniesByID identifierS (x:xs)
    | ident x == identifierS = x
    | otherwise = getCompaniesByID identifierS xs


-- Remove uma empresa pelo ID
removeCompanyByID :: Int -> [Company] -> [Company]
removeCompanyByID _ [] = []
removeCompanyByID identifierS (x:xs)
    | ident x == identifierS = xs
    | otherwise = x : removeCompanyByID identifierS xs


-- Pega todas as empresas salvas
getCompanyJSON :: String -> [Company]
getCompanyJSON path = do
    let file = unsafePerformIO (B.readFile path)
        decodedFile = decode file :: Maybe [Company]
    case decodedFile of
        Nothing -> []
        Just out -> out


-- Salva uma Empresa
saveCompanyJSON :: String -> Company -> IO ()
saveCompanyJSON jsonFilePath company = do
    let companyList = getCompanyJSON jsonFilePath
        newID = identifyIDSequenceBreak [1 .. 12]
        companiesList = companyList ++ [giveIdForCompany company newID]

    textoContents <- readFile "./Sprites/HomeBroker/homebroker_base.txt"
    let walletFileName = "./Models/Company/HomeBrokers/homebroker" ++ show newID ++ ".txt"
    appendFile walletFileName textoContents

    B.writeFile "./Data/ArquivoTemporario.json" $ encode companiesList
    removeFile jsonFilePath
    renameFile "./Data/ArquivoTemporario.json" jsonFilePath


-- Identifica uma quebra na sequência de IDs das empresas, retornando o primeiro valor ausente
identifyIDSequenceBreak :: [Int] -> Int
identifyIDSequenceBreak [] = 1
identifyIDSequenceBreak (x:xs)
    | x `notElem` ids = x
    | otherwise = identifyIDSequenceBreak xs
    where
        ids = sort [ident x | x <- getCompanyJSON "./Data/Companies.json"]


-- Edita as ações da Empresa
editCompanyJSON :: String -> Company -> IO ()
editCompanyJSON jsonFilePath updatedCompany = do
    let companiesList = getCompanyJSON jsonFilePath
        newCompaniesList = removeCompanyByID (ident updatedCompany) companiesList ++ [updatedCompany]
    B.writeFile "./Data/ArquivoTemporario.json" $ encode newCompaniesList
    removeFile jsonFilePath
    renameFile "./Data/ArquivoTemporario.json" jsonFilePath


-- Remove uma empresa pelo ID
removeCompany :: Int -> String -> IO ()
removeCompany idComp jsonFilePath = do
    let companiesList = getCompanyJSON jsonFilePath
        newCompaniesList = removeCompanyByID idComp companiesList
    B.writeFile "./Data/ArquivoTemporario.json" $ encode newCompaniesList
    removeFile jsonFilePath
    renameFile "./Data/ArquivoTemporario.json" jsonFilePath
    let filePathToDelete = "./Models/Company/HomeBrokers/homebroker" ++ show idComp ++ ".txt"
    removeFile filePathToDelete


-- Verifica a existencia do cliente pelo email
existCompanyByName :: String -> Bool
existCompanyByName name = verifyExistNameCompany name (getCompanyJSON "./Data/Clients.json")


verifyExistNameCompany :: String -> [Company] -> Bool
verifyExistNameCompany _ [] = False
verifyExistNameCompany nameCompany (head:tail)
    | nameCompany == name head = True
    | otherwise = verifyExistNameCompany nameCompany tail


-- Atribui novo id ao cliente
giveIdForCompany :: Company -> Int -> Company
giveIdForCompany client newId = client { ident = newId }