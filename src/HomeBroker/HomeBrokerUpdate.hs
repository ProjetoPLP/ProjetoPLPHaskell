module HomeBroker.HomeBrokerUpdate where

import Utils.MatrixUtils (writeMatrixValue)
import Utils.UpdateUtils (fillLeft)

updateHBStockPrice :: FilePath -> Float -> String -> IO ()
updateHBStockPrice filePath num trendInd = do
    let val = fillLeft (trendInd ++ show num) 3
    writeMatrixValue filePath val 11 (94 - length val)


updateHBGraphCandle :: FilePath -> Int -> Int -> IO ()
updateHBGraphCandle filePath row col = do
    writeMatrixValue filePath "❚" row col


-- Reinicia o gráfico sobrescrevendo todos os espaços com caracteres vazios
cleanHBGraph :: FilePath -> Int -> IO ()
cleanHBGraph filepath 26 = writeMatrixValue filepath (replicate 74 ' ') 26 2
cleanHBGraph filepath row = do
    writeMatrixValue filepath (replicate 74 ' ') row 2
    cleanHBGraph filepath (row + 1)


updateHBStockMaxPrice :: FilePath -> Float -> IO ()
updateHBStockMaxPrice filePath num = do
    let val = fillLeft (show num) 4
    writeMatrixValue filePath val 16 (94 - length val)


updateHBStockMinPrice :: FilePath -> Float -> IO ()
updateHBStockMinPrice filePath num = do
    let val = fillLeft (show num) 4
    writeMatrixValue filePath val 18 (94 - length val)


updateHBStockStartPrice :: FilePath -> Float -> IO ()
updateHBStockStartPrice filePath num = do
    let val = fillLeft (show num) 4
    writeMatrixValue filePath val 14 (94 - length val)


updateHBCash :: FilePath -> Float -> IO ()
updateHBCash filePath num = do
    let val = fillLeft (show num) 8
    writeMatrixValue filePath val 3 (76 - length val)

updateHBOwnedStocks :: FilePath -> Int -> IO ()
updateHBOwnedStocks filePath num = do
    let val = fillLeft (show num) 5
    writeMatrixValue filePath val 21 (95 - length val)


updateHBStockName :: FilePath -> String -> IO ()
updateHBStockName filePath name = do
    writeMatrixValue filePath name 3 47


updateHBCompanyName :: FilePath -> String -> IO ()
updateHBCompanyName filePath name = do
    writeMatrixValue filePath name 7 (getCompanyNameCol (length name))


-- Formata, a partir do tamanho do nome, a coluna na qual o nome será escrito 
getCompanyNameCol :: Int -> Int
getCompanyNameCol len = 86 - ((len - 1) `div` 2)
