module HomeBroker.CompanyProfile.CompanyProfileUpdate where

import Utils.UpdateUtils (fillLeft, resetMenu)
import Utils.MatrixUtils (writeMatrixValue)

import Clock.ClockUpdate (updateMatrixClock)

import Company.GetSetAttrsCompany (getCode, getName, getActuation, getPrice, getTrendIndicator, getDeclaration, getAge, getCNPJ)
import Client.GetSetAttrsClient (getCash)


-- Atualiza todas as informações de uma empresa em Company Description
updateCompanyProfile :: Int -> Int -> IO ()
updateCompanyProfile idClient idComp = do
    resetMenu filePath "./Sprites/HomeBroker/companyProfile_base.txt"
    updateMatrixClock filePath
    updateCPCash filePath (getCash idClient)
    updateCPCompanyCode filePath (getCode idComp)
    updateCPCompanyName filePath (getName idComp)
    updateCPCompanyActuation filePath (getActuation idComp)
    updateCPCompanyPrice filePath (getPrice idComp) (getTrendIndicator idComp)
    updateCPCompanyDeclaration filePath (getDeclaration idComp)
    updateCPCompanyAge filePath (getAge idComp)
    updateCPCompanyCNPJ filePath (getCNPJ idComp)
    where filePath = "./HomeBroker/CompanyProfile/companyProfile.txt"


updateCPCash :: FilePath -> Float -> IO ()
updateCPCash filePath num = do
    let val = fillLeft (show num) 8
    writeMatrixValue filePath val 3 (74 - length val)


updateCPCompanyCode :: FilePath -> String -> IO ()
updateCPCompanyCode filePath code = do
    writeMatrixValue filePath code 9 6


updateCPCompanyName :: FilePath -> String -> IO ()
updateCPCompanyName filePath name = do
    writeMatrixValue filePath name 9 14


updateCPCompanyActuation :: FilePath -> String -> IO ()
updateCPCompanyActuation filePath actuation = do
    writeMatrixValue filePath actuation 9 46


updateCPCompanyPrice :: FilePath -> Float -> String -> IO ()
updateCPCompanyPrice filePath price trendInd = do
    let val = fillLeft (trendInd ++ show price ++ "0") 7
    writeMatrixValue filePath val 9 (93 - length val)


updateCPCompanyDeclaration :: FilePath -> String -> IO ()
updateCPCompanyDeclaration filePath declaration = do
    writeMatrixValue filePath declaration 17 8


updateCPCompanyAge :: FilePath -> Int -> IO ()
updateCPCompanyAge filePath age = do
    writeMatrixValue filePath (show age) 20 23


updateCPCompanyCNPJ :: FilePath -> String -> IO ()
updateCPCompanyCNPJ filePath cnpj = do
    writeMatrixValue filePath cnpj 23 12