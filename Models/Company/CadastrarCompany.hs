module Models.Company.CadastrarCompany where

import System.Random ( randomRIO )
import Utils.UpdateUtils ( format )
import Models.Company.SaveCompany ( saveCompanyJSON )
import Models.Company.ModelCompany ( createCompany, Company )
import Models.Company.GetInfoForCreateCompany ( getAgeFounded, getCNPJ, getActuation, getDeclaration, getCode, getName )


cadastrarCompany :: Int -> IO Bool
cadastrarCompany limitCompanies = do
    if limitCompanies < 12 then do
        company <- getNewCompany
        saveCompanyJSON "./Data/Companies.json" company
        return True
    else return False


getNewCompany :: IO Company
getNewCompany = do
    companyName <- getName
    companyAgeFounded <- getAgeFounded
    companyCNPJ <- getCNPJ
    companyActuation <- getActuation
    companyDeclaration <- getDeclaration
    companyCode <- getCode
    companyPrice <- randomCompanyPrice
    return $ createCompany 10 companyName companyAgeFounded companyCNPJ companyActuation companyDeclaration companyCode companyPrice " "


-- Retorna um preço aleatório entre 10.0 e 30.0 
randomCompanyPrice :: IO Float
randomCompanyPrice = do
    newPrice <- randomRIO (10,30 :: Float)
    return (format newPrice)