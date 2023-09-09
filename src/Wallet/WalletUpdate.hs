module Wallet.WalletUpdate where

import Utils.MatrixUtils (writeMatrixValue)
import Utils.UpdateUtils (fillLeft, fillRight)

import Client.GetSetAttrsClient

updateClientWallet :: Int -> IO ()
updateClientWallet idClient = do
    
    updateWLUserName path (getName idClient)
    updateWLCash path (getCash idClient)
    updateWLPatrimony path (getPatrimony idClient)
    updateWLUserName path (getName idClient)
    updateWLUserCPF path (getCPF idClient)
    where path = "./Client/Wallet/wallet" ++ show idClient ++ ".txt"

updateWLCash :: FilePath -> Float -> IO ()
updateWLCash filePath num = do
    let val = fillLeft (show num) 8
    writeMatrixValue filePath val 13 (21 - length val)


updateWLPatrimony :: FilePath -> Float -> IO ()
updateWLPatrimony filePath num = do
    let val = fillLeft (show num) 8
    writeMatrixValue filePath val 6 (23 - length val)


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


updateWLStockName :: FilePath -> Int -> String -> IO ()
updateWLStockName filePath id name = do
    let pos = getStockNamePosition id
    writeMatrixValue filePath name (head pos) (last pos)


updateWLStockPrice :: FilePath -> Int -> Float -> String -> IO ()
updateWLStockPrice filePath id num trendInd = do
    let pos = getStockPricePosition id
        val = fillLeft (trendInd ++ show num) 6
    writeMatrixValue filePath val (head pos) (last pos - length val)


updateWLOwnedStock :: FilePath -> Int -> Int -> IO ()
updateWLOwnedStock filePath id num = do
    let pos = getOwnedStockPosition id
        val = fillLeft (show num) 5
    writeMatrixValue filePath val (head pos) (last pos - length val)


updateWLUserName :: FilePath -> String -> IO ()
updateWLUserName filePath name = do
    writeMatrixValue filePath name 10 6


updateWLUserCPF :: FilePath -> String -> IO ()
updateWLUserCPF filePath name = do
    writeMatrixValue filePath name 11 6


updateWLNewsPercent :: FilePath -> Float -> IO ()
updateWLNewsPercent filePath num = do
    let val = fillLeft (show num) 4
    writeMatrixValue filePath val 16 (33 - length val)


updateWLNewsText :: FilePath -> String -> IO ()
updateWLNewsText filePath text = do
    let val = fillRight text 3
    writeMatrixValue filePath val 14 29


getStockNamePosition :: Int -> [Int]
getStockNamePosition id
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


getStockPricePosition :: Int -> [Int]
getStockPricePosition id
    | id == 1 = [22, 15]
    | id == 2 = [24, 15]
    | id == 3 = [26, 15]
    | id == 4 = [22, 39]
    | id == 5 = [24, 39]
    | id == 6 = [26, 39]
    | id == 7 = [22, 63]
    | id == 8 = [24, 63]
    | id == 9 = [26, 63]
    | id == 10 = [22, 87]
    | id == 11 = [24, 87]
    | id == 12 = [26, 87]


getOwnedStockPosition :: Int -> [Int]
getOwnedStockPosition id
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