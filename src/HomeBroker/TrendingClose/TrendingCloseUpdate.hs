module HomeBroker.TrendingClose.TrendingCloseUpdate where
import Utils.MatrixUtils (writeMatrixValue)
import Utils.UpdateUtils (fillLeft, resetMenu)
import Company.GetSetAttrsCompany (getStartPrice, getPrice, getCode, getIdent, setStartPrice)
import Company.ModelCompany (Company)
import Client.GetSetAttrsClient (getCash, getPatrimony)
import Company.SaveCompany (getCompanyJSON)
import Clock.ClockUpdate (updateMatrixClock)


updateTrendingClose :: Int -> IO ()
updateTrendingClose idUser = do
    resetMenu filePath "./Sprites/HomeBroker/trendingClose_base.txt"
    updateMatrixClock filePath
    updateTCCash filePath (getCash idUser)
    updateTCPatrimony filePath (getPatrimony idUser)
    updateAllTCCompanyCode filePath jsonPath
    updateAllTCCompanyVar filePath jsonPath
    updateAllCompaniesStartPrice jsonPath
    where filePath = "./HomeBroker/TrendingClose/trendingClose.txt"
          jsonPath = getCompanyJSON "./Data/Companies.json"


updateTCCash :: FilePath -> Float -> IO ()
updateTCCash filePath num = do
    let val = fillLeft (show num ++ "0") 9
    writeMatrixValue filePath val 3 (75 - length val)


updateTCPatrimony :: FilePath -> Float -> IO ()
updateTCPatrimony filePath patri = do
    let val = fillLeft (show patri ++ "0") 9
    writeMatrixValue filePath val 3 (40 - length val)


updateAllTCCompanyCode :: FilePath -> [Company] -> IO ()
updateAllTCCompanyCode filePath [] = return ()
updateAllTCCompanyCode filePath (x:xs) = do
    updateTCCompanyCode filePath (getIdent x)
    updateAllTCCompanyCode filePath xs


updateTCCompanyCode :: FilePath -> Int -> IO ()
updateTCCompanyCode filePath id = do
    let pos = getCompanyCodePosition id
    writeMatrixValue filePath (getCode id) (head pos) (last pos)


updateAllTCCompanyVar :: FilePath -> [Company] -> IO ()
updateAllTCCompanyVar filePath [] = return ()
updateAllTCCompanyVar filePath (x:xs) = do
    updateTCCompanyVar filePath (getIdent x)
    updateAllTCCompanyVar filePath xs


updateTCCompanyVar :: FilePath -> Int -> IO ()
updateTCCompanyVar filePath id = do
    let pos = getCompanyVarPosition id
        val = fillLeft (getVar id) 8
    writeMatrixValue filePath val (head pos) (last pos - length val)


updateAllCompaniesStartPrice :: [Company] -> IO ()
updateAllCompaniesStartPrice [] = return ()
updateAllCompaniesStartPrice (x:xs) = do
    let idComp = getIdent x
    setStartPrice idComp (getPrice idComp)
    updateAllCompaniesStartPrice xs


getVar :: Int -> String
getVar idComp
    | var > 0 = "▲" ++ show (format var) ++ "0%"
    | var < 0 = "▼" ++ tail (show (format var)) ++ "0%"
    | otherwise = "0.0%"
    where
        format :: Float -> Float
        format var = fromIntegral (round (var * 10 )) / 10

        var = ((getPrice idComp - getStartPrice idComp) / getStartPrice idComp) * 100
            

getCompanyCodePosition :: Int -> [Int]
getCompanyCodePosition id
    | id == 1 = [13, 12]
    | id == 2 = [16, 12]
    | id == 3 = [19, 12]
    | id == 4 = [22, 12]
    | id == 5 = [13, 43]
    | id == 6 = [16, 43]
    | id == 7 = [19, 43]
    | id == 8 = [22, 43]
    | id == 9 = [13, 74]
    | id == 10 = [16, 74]
    | id == 11 = [19, 74]
    | id == 12 = [22, 74]


getCompanyVarPosition :: Int -> [Int]
getCompanyVarPosition id
    | id == 1 = [13, 27]
    | id == 2 = [16, 27]
    | id == 3 = [19, 27]
    | id == 4 = [22, 27]
    | id == 5 = [13, 58]
    | id == 6 = [16, 58]
    | id == 7 = [19, 58]
    | id == 8 = [22, 58]
    | id == 9 = [13, 89]
    | id == 10 = [16, 89]
    | id == 11 = [19, 89]
    | id == 12 = [22, 89]
