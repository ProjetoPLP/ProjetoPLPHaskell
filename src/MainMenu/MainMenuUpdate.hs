module MainMenu.MainMenuUpdate where
import Utils.UpdateUtils
import Utils.MatrixUtils (writeMatrixValue)
import Clock.ClockUpdate (updateMatrixClock)
import Clock.GetSetClock (getClock)
import Client.GetSetAttrsClient (getCash)
import Company.GetSetAttrsCompany (getCode, getIdent, getName, getPrice, getTrendIndicator)
import Company.ModelCompany (Company)
import Company.SaveCompany (getCompanyJSON)


updateMainMenu :: Int -> IO ()
updateMainMenu idClient = do
    updateMatrixClock filePath getClock
    updateMMCash filePath (getCash idClient)
    updateAllMMCompanyCode filePath jsonPath
    updateAllMMCompanyPrice filePath jsonPath
    updateAllMMCompanyName filePath jsonPath
    where filePath = "./MainMenu/mainMenu.txt"
          jsonPath = getCompanyJSON "./Data/Companies.json"


updateMMCash :: FilePath -> Float -> IO ()
updateMMCash filePath num = do
    let val = fillLeft (show num) 8
    writeMatrixValue filePath val 3 (74 - length val)


updateAllMMCompanyCode :: FilePath -> [Company] -> IO ()
updateAllMMCompanyCode filePath [] = return ()
updateAllMMCompanyCode filePath (x:xs) = do
    updateMMCompanyCode filePath (getIdent x)
    updateAllMMCompanyCode filePath xs


updateMMCompanyCode :: FilePath -> Int -> IO ()
updateMMCompanyCode filePath id = do
    let pos = getCompanyCodePosition id
    writeMatrixValue filePath (getCode id) (head pos) (last pos)


updateAllMMCompanyName :: FilePath -> [Company] -> IO ()
updateAllMMCompanyName filePath [] = return ()
updateAllMMCompanyName filePath (x:xs) = do
    let id = getIdent x
    updateMMCompanyName filePath id (getName id)
    updateAllMMCompanyName filePath xs


updateMMCompanyName :: FilePath -> Int -> String -> IO ()
updateMMCompanyName filePath id name = do
    let pos = getCompanyNamePosition id
    writeMatrixValue filePath name (head pos) (getCompanyNameCol (length name) (last pos))


updateAllMMCompanyPrice :: FilePath -> [Company] -> IO ()
updateAllMMCompanyPrice filePath [] = return ()
updateAllMMCompanyPrice filePath (x:xs) = do
    let id = getIdent x
    updateMMCompanyPrice filePath id (getPrice id) (getTrendIndicator id)
    updateAllMMCompanyPrice filePath xs


updateMMCompanyPrice :: FilePath -> Int -> Float -> String -> IO ()
updateMMCompanyPrice filePath id num trendInd = do
    let pos = getCompanyPricePosition id
        val = fillLeft (trendInd ++ show num) 6
    writeMatrixValue filePath val (head pos) (last pos - length val)


getCompanyNameCol :: Int -> Int -> Int
getCompanyNameCol len col = col - ((len - 1) `div` 2)

getCompanyCodePosition :: Int -> [Int]
getCompanyCodePosition id
    | id == 1 = [8, 24]
    | id == 2 = [13, 24]
    | id == 3 = [18, 24]
    | id == 4 = [23, 24]
    | id == 5 = [8, 49]
    | id == 6 = [13, 49]
    | id == 7 = [18, 49]
    | id == 8 = [23, 49]
    | id == 9 = [8, 74]
    | id == 10 = [13, 74]
    | id == 11 = [18, 74]
    | id == 12 = [23, 74]

getCompanyNamePosition :: Int -> [Int]
getCompanyNamePosition id
    | id == 1 = [9, 31]
    | id == 2 = [14, 31]
    | id == 3 = [19, 31]
    | id == 4 = [24, 31]
    | id == 5 = [9, 56]
    | id == 6 = [14, 56]
    | id == 7 = [19, 56]
    | id == 8 = [24, 56]
    | id == 9 = [9, 81]
    | id == 10 = [14, 81]
    | id == 11 = [19, 81]
    | id == 12 = [24, 81]

getCompanyPricePosition :: Int -> [Int]
getCompanyPricePosition id
    | id == 1 = [8, 38]
    | id == 2 = [13, 38]
    | id == 3 = [18, 38]
    | id == 4 = [23, 38]
    | id == 5 = [8, 63]
    | id == 6 = [13, 63]
    | id == 7 = [18, 63]
    | id == 8 = [23, 63]
    | id == 9 = [8, 88]
    | id == 10 = [13, 88]
    | id == 11 = [18, 88]
    | id == 12 = [23, 88]
