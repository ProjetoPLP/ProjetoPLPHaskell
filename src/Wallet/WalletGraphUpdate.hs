module Wallet.WalletGraphUpdate where

import Utils.MatrixUtils (writeMatrixValue)

updateWLGraphCandle :: FilePath -> Int -> Int -> IO ()
updateWLGraphCandle filePath row col = do
    writeMatrixValue filePath "|" row col


-- refazer para wallet
-- Reinicia o gráfico sobrescrevendo todos os espaços com caracteres vazios
cleanHBGraph :: FilePath -> Int -> IO ()
cleanHBGraph filepath 26 = writeMatrixValue filepath (replicate 74 ' ') 26 2
cleanHBGraph filepath row = do
    writeMatrixValue filepath (replicate 74 ' ') row 2
    cleanHBGraph filepath (row + 1)