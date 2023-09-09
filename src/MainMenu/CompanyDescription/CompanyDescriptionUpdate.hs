module MainMenu.CompanyDescription.CompanyDescriptionUpdate where
import Utils.UpdateUtils
import Utils.MatrixUtils (writeMatrixValue)
import Clock.ClockUpdate (updateMatrixClock)
import Clock.GetSetClock (getClock)
import Company.SaveCompany (getCompanyJSON)
import Client.GetSetAttrsClient (getCash)
import Company.GetSetAttrsCompany (getCode, getName, getActuation, getPrice, getTrendIndicator, getDeclaration, getAge, getCNPJ)


-- Atualiza toddas as informações de uma empresa em Company Description
updateCompanyDescription :: Int -> Int -> IO ()
updateCompanyDescription idClient idComp = do
    resetMenu filePath "./Sprites/companyDescription_base.txt"
    updateMatrixClock filePath
    updateCDCash filePath (getCash idClient)
    updateCDCompanyCode filePath (getCode idComp)
    updateCDCompanyName filePath (getName idComp)
    updateCDCompanyActuation filePath (getActuation idComp)
    updateCDCompanyPrice filePath (getPrice idComp) (getTrendIndicator idComp)
    updateCDCompanyDeclaration filePath (getDeclaration idComp)
    updateCDCompanyAge filePath (getAge idComp)
    updateCDCompanyCNPJ filePath (getCNPJ idComp)
    where filePath = "./MainMenu/CompanyDescription/companyDescription.txt"


updateCDCash :: FilePath -> Float -> IO ()
updateCDCash filePath num = do
    let val = fillLeft (show num) 8
    writeMatrixValue filePath val 3 (74 - length val)


updateCDCompanyCode :: FilePath -> String -> IO ()
updateCDCompanyCode filePath code = do
    writeMatrixValue filePath code 9 6


updateCDCompanyName :: FilePath -> String -> IO ()
updateCDCompanyName filePath name = do
    writeMatrixValue filePath name 9 14


updateCDCompanyActuation :: FilePath -> String -> IO ()
updateCDCompanyActuation filePath actuation = do
    writeMatrixValue filePath actuation 9 61


updateCDCompanyPrice :: FilePath -> Float -> String -> IO ()
updateCDCompanyPrice filePath price trendInd = do
    let val = fillLeft (trendInd ++ show price) 6
    writeMatrixValue filePath val 9 (92 - length val)


updateCDCompanyDeclaration :: FilePath -> String -> IO ()
updateCDCompanyDeclaration filePath declaration = do
    writeMatrixValue filePath declaration 17 8


updateCDCompanyAge :: FilePath -> Int -> IO ()
updateCDCompanyAge filePath age = do
    writeMatrixValue filePath (show age) 20 23


updateCDCompanyCNPJ :: FilePath -> String -> IO ()
updateCDCompanyCNPJ filePath cnpj = do
    writeMatrixValue filePath cnpj 23 12