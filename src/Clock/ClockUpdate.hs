module Clock.ClockUpdate where

import Utils.MatrixUtils
import Utils.UpdateUtils

updateMatrixClock :: FilePath -> Int -> IO ()
updateMatrixClock filePath num = do
    let val = getHour num
    writeValue filePath val 3 88


getHour :: Int -> String
getHour num
    | num `div` 60 < 10 && num `mod` 60 < 10 = "0" ++ show (num `div` 60) ++ ":0" ++ show (num `mod` 60)
    | num `div` 60 < 10 && num `mod` 60 >= 10 = "0" ++ show (num `div` 60) ++ ":" ++ show (num `mod` 60)
    | num `div` 60 >= 10 && num `mod` 60 < 10 = show (num `div` 60) ++ ":0" ++ show (num `mod` 60)
    | otherwise = show (num `div` 60) ++ ":" ++ show (num `mod` 60)
    