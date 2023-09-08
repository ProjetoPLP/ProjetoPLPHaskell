module Wallet.WalletUpdate where

import Utils.MatrixUtils (writeMatrixValue)
import Utils.UpdateUtils (fillLeft, fillRight)


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


updateWLStockPrice :: FilePath -> Int -> Float -> IO ()
updateWLStockPrice filePath id num = do
    let pos = getStockPricePosition id
        val = fillLeft (show num) 4
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
    | id == 1 = [22, 4]
    | id == 2 = [24, 4]
    | id == 3 = [26, 4]
    | id == 4 = [22, 28]
    | id == 5 = [24, 28]
    | id == 6 = [26, 28]
    | id == 7 = [22, 52]
    | id == 8 = [24, 52]
    | id == 9 = [26, 52]
    | id == 10 = [22, 76]
    | id == 11 = [24, 76]
    | id == 12 = [26, 76]


getStockPricePosition :: Int -> [Int]
getStockPricePosition id
    | id == 1 = [22, 14]
    | id == 2 = [24, 14]
    | id == 3 = [26, 14]
    | id == 4 = [22, 38]
    | id == 5 = [24, 38]
    | id == 6 = [26, 38]
    | id == 7 = [22, 62]
    | id == 8 = [24, 62]
    | id == 9 = [26, 62]
    | id == 10 = [22, 86]
    | id == 11 = [24, 86]
    | id == 12 = [26, 86]


getOwnedStockPosition :: Int -> [Int]
getOwnedStockPosition id
    | id == 1 = [22, 22]
    | id == 2 = [24, 22]
    | id == 3 = [26, 22]
    | id == 4 = [22, 46]
    | id == 5 = [24, 46]
    | id == 6 = [26, 46]
    | id == 7 = [22, 70]
    | id == 8 = [24, 70]
    | id == 9 = [26, 70]
    | id == 10 = [22, 94]
    | id == 11 = [24, 94]
    | id == 12 = [26, 94]