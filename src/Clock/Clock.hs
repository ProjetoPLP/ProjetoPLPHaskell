module Clock.Clock where

import Utils.MatrixUtils
import Utils.UpdateUtils

updateMatrixClock :: FilePath -> Int -> IO ()
updateMatrixClock filePath num = do
    let val = getHour (if num >= 300 then 0 else num)
    writeValue filePath val 3 88


getHour :: Int -> String
getHour num
    | newHour `div` 60 < 10 && newHour `mod` 60 < 10 = "0" ++ show (newHour `div` 60) ++ ":0" ++ show (newHour `mod` 60)
    | newHour `div` 60 < 10 && newHour `mod` 60 >= 10 = "0" ++ show (newHour `div` 60) ++ ":" ++ show (newHour `mod` 60)
    | newHour `div` 60 >= 10 && newHour `mod` 60 < 10 = show (newHour `div` 60) ++ ":0" ++ show (newHour `mod` 60)
    | otherwise = show (newHour `div` 60) ++ ":" ++ show (newHour `mod` 60)
    where newHour = num + 420