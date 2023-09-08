module Wallet.WalletGraphUpdate where

import Utils.MatrixUtils (writeMatrixValue)

updateWLGraphCandle :: FilePath -> Int -> Int -> IO ()
updateWLGraphCandle filePath row col = do
    writeMatrixValue filePath "|" row col