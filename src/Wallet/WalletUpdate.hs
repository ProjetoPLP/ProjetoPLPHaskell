module Wallet.WalletUpdate where

import Utils.MatrixUtils (writeMatrixValue)
import Utils.UpdateUtils (fillLeft, fillRight, resetMenu)

import Client.GetSetAttrsClient as Cli ( getCPF, getCash, getName, getPatrimony )

import Clock.ClockUpdate
import Company.GetSetAttrsCompany as Com (getCode, getIdent, getName, getPrice, getTrendIndicator)
import Company.ModelCompany (Company)
import Company.SaveCompany (getCompanyJSON)


-- Aualiza todas as informações da carteira de cliente
updateClientWallet :: Int -> IO ()
updateClientWallet idClient = do
    resetMenu filePath "./Sprites/wallet_base.txt"
    updateMatrixClock filePath
    updateWLUserName filePath (Cli.getName idClient)
    updateWLCash filePath (getCash idClient)
    updateWLPatrimony filePath (getPatrimony idClient)
    updateWLUserName filePath (Cli.getName idClient)
    updateWLUserCPF filePath (getCPF idClient)
    updateAllWLCompanyCode filePath jsonPath
    updateAllWLCompanyPrice filePath jsonPath
    -- updateAllWLOwnedStocks
    where filePath = "./Client/Wallet/wallet" ++ show idClient ++ ".txt"
          jsonPath = getCompanyJSON "./Data/Companies.json"

updateWLCash :: FilePath -> Float -> IO ()
updateWLCash filePath cash = do
    let val = fillLeft (show cash ++ "0") 9
    writeMatrixValue filePath val 13 (22 - length val)


updateWLPatrimony :: FilePath -> Float -> IO ()
updateWLPatrimony filePath patri = do
    let val = fillLeft (show patri ++ "0") 9
    writeMatrixValue filePath val 6 (24 - length val)


updateWLGraphCandle :: FilePath -> Int -> Int -> IO ()
updateWLGraphCandle filePath row col = do
    writeMatrixValue filePath "❚" row col


-- refazer para wallet
-- Reinicia o gráfico sobrescrevendo todos os espaços com caracteres vazios
cleanWLGraph :: FilePath -> Int -> IO ()
cleanWLGraph filepath 20 = writeMatrixValue filepath (replicate 47 ' ') 20 50
cleanWLGraph filepath row = do
    writeMatrixValue filepath (replicate 47 ' ') row 50
    cleanWLGraph filepath (row + 1)


updateAllWLCompanyCode :: FilePath -> [Company] -> IO ()
updateAllWLCompanyCode filePath [] = return ()
updateAllWLCompanyCode filePath (x:xs) = do
    let id = getIdent x
    updateWLCompanyCode filePath id (Com.getCode id)
    updateAllWLCompanyCode filePath xs


updateWLCompanyCode :: FilePath -> Int -> String -> IO ()
updateWLCompanyCode filePath id code = do
    let pos = getCompanyCodePosition id
    writeMatrixValue filePath code (head pos) (last pos)



updateAllWLCompanyPrice :: FilePath -> [Company] -> IO ()
updateAllWLCompanyPrice filePath [] = return ()
updateAllWLCompanyPrice filePath (x:xs) = do
    let id = getIdent x
    updateWLCompanyPrice filePath id (getPrice id) (getTrendIndicator id)
    updateAllWLCompanyPrice filePath xs


updateWLCompanyPrice :: FilePath -> Int -> Float -> String -> IO ()
updateWLCompanyPrice filePath id price trendInd = do
    let pos = getCompanyPricePosition id
        val = fillLeft (trendInd ++ show price ++ "0") 7
    writeMatrixValue filePath val (head pos) (last pos - length val)


updateWLOwnedStocks :: FilePath -> Int -> Int -> IO ()
updateWLOwnedStocks filePath id num = do
    let pos = getOwnedStocksPosition id
        val = fillLeft (show num) 5
    writeMatrixValue filePath val (head pos) (last pos - length val)


updateWLUserName :: FilePath -> String -> IO ()
updateWLUserName filePath name = do
    writeMatrixValue filePath name 10 6


updateWLUserCPF :: FilePath -> String -> IO ()
updateWLUserCPF filePath cpf = do
    writeMatrixValue filePath cpf 11 6


updateWLNewsPercent :: FilePath -> Float -> IO ()
updateWLNewsPercent filePath perc = do
    let val = fillLeft (show perc) 4
    writeMatrixValue filePath val 16 (33 - length val)


updateWLNewsText :: FilePath -> String -> IO ()
updateWLNewsText filePath text = do
    let val = fillRight text 3
    writeMatrixValue filePath val 14 29


getCompanyCodePosition :: Int -> [Int]
getCompanyCodePosition id
    | id == 1 = [22, 3]
    | id == 2 = [24, 3]
    | id == 3 = [26, 3]
    | id == 4 = [22, 27]
    | id == 5 = [24, 27]
    | id == 6 = [26, 27]
    | id == 7 = [22, 51]
    | id == 8 = [24, 51]
    | id == 9 = [26, 51]
    | id == 10 = [22, 75]
    | id == 11 = [24, 75]
    | id == 12 = [26, 75]


getCompanyPricePosition :: Int -> [Int]
getCompanyPricePosition id
    | id == 1 = [22, 16]
    | id == 2 = [24, 16]
    | id == 3 = [26, 16]
    | id == 4 = [22, 40]
    | id == 5 = [24, 40]
    | id == 6 = [26, 40]
    | id == 7 = [22, 64]
    | id == 8 = [24, 64]
    | id == 9 = [26, 64]
    | id == 10 = [22, 88]
    | id == 11 = [24, 88]
    | id == 12 = [26, 88]


getOwnedStocksPosition :: Int -> [Int]
getOwnedStocksPosition id
    | id == 1 = [22, 23]
    | id == 2 = [24, 23]
    | id == 3 = [26, 23]
    | id == 4 = [22, 47]
    | id == 5 = [24, 47]
    | id == 6 = [26, 47]
    | id == 7 = [22, 71]
    | id == 8 = [24, 71]
    | id == 9 = [26, 71]
    | id == 10 = [22, 95]
    | id == 11 = [24, 95]
    | id == 12 = [26, 95]