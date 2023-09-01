module Wallet.WalletGraphUpdate where

import Utils.MatrixUtils
import Utils.UpdateUtils

updateWLGraphCandle :: FilePath -> Int -> Int -> IO ()
updateWLGraphCandle filePath row col = do
    writeValue filePath "|" row col