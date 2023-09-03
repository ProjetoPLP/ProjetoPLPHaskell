module HomeBroker.HomeBrokerGraphUpdate where

import Control.Concurrent (threadDelay)
import System.Random (randomRIO)
import Data.Time.Clock (getCurrentTime, UTCTime, addUTCTime)

import HomeBroker.HomeBrokerUpdate
import Utils.MatrixUtils (printMatrix, writeValue)

import Company.GetSetAttrsCompany
import Clock.ClockUpdate
import Clock.GetSetClock (setClock, readClock)

updateHBGraphCandle :: FilePath -> Int -> Int -> IO ()
updateHBGraphCandle filePath row col = do
    writeValue filePath "❚" row col

getIndex :: IO Int
getIndex = do
    index <- randomRIO (0,9 :: Int)
    return index

getVariation :: IO Int
getVariation = do
    var <- randomRIO (-1,1 :: Int)
    return var


getNewPrice :: IO Float
getNewPrice = do
    index <- getIndex
    var <- getVariation
    if var == 1 then return ([0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0] !! index)
    else if var == (-1) then return ([-0.1, -0.2, -0.3, -0.4, -0.5, -0.6, -0.7, -0.8, -0.9, -1.0] !! index)
    else return 0


attStockPriceFor :: Int -> Int -> IO ()
attStockPriceFor id seg = do
    startTime <- getCurrentTime
    setClock seg
    newClockHour <- readClock
    updateMatrixClock ("./Company/HomeBroker/homebroker" ++ show id ++ ".txt") newClockHour
    let endTime = addUTCTime (fromIntegral seg) startTime
    loop id endTime

loop :: Int -> UTCTime -> IO ()
loop id endTime = do
    currentTime <- getCurrentTime
    if currentTime >= endTime then do
        updateCol id 3
        putStrLn "Tempo esgotado."
        else do
            attStocksPrice id
            threadDelay (1 * 500000)
            loop id endTime

attStocksPrice :: Int -> IO ()
attStocksPrice id = do
    newPrice <- getNewPrice
    if newPrice + getSaldo id > getSaldo id then do
        if getRow id == 6 then do
            cleanGraph ("./Company/HomeBroker/homebroker" ++ show id ++ ".txt") 6
            updateRow id 20
        else
            updateRow id (-1)
    else do
        if getRow id == 26 then do
            cleanGraph ("./Company/HomeBroker/homebroker" ++ show id ++ ".txt") 6
            updateRow id (-20)
        else
            updateRow id 1
    setSaldo id newPrice
    updateHBStockPrice ("./Company/HomeBroker/homebroker" ++ show id ++ ".txt") (getSaldo id)
    updateHBGraphCandle ("./Company/HomeBroker/homebroker" ++ show id ++ ".txt") (getRow id) (getCol id)
    printMatrix ("./Company/HomeBroker/homebroker" ++ show id ++ ".txt")

cleanGraph :: FilePath -> Int -> IO ()
cleanGraph filepath 26 = writeValue filepath (replicate 74 ' ') 26 2
cleanGraph filepath row = do
    writeValue filepath (replicate 74 ' ') row 2
    cleanGraph filepath (row + 1) 
