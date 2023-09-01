module HomeBroker.HomeBrokerGraphUpdate where

import Utils.MatrixUtils

updateHBGraphCandle :: FilePath -> Int -> Int -> IO ()
updateHBGraphCandle filePath row col = do
    writeValue filePath "|" row col