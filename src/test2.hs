import Control.Concurrent (threadDelay)
import System.Random (randomRIO)
import Data.Time.Clock (getCurrentTime, UTCTime, addUTCTime)

import HomeBroker.HomeBrokerUpdate
import Utils.MatrixUtils (printMatrix)

import Company.GetSetAttrsCompany (getSaldo, setSaldo)

getIndex :: IO Int
getIndex = do
    index <- randomRIO (0,9 :: Int)
    return index

getVariation :: IO Int
getVariation = do
    var <- randomRIO (-1,1 :: Int)
    return var


getNewPriceOld :: IO Float
getNewPriceOld = do
    index <- getIndex
    var <- getVariation
    if var == 1 then return ([0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0] !! index)
    else if var == (-1) then return ([-0.1, -0.2, -0.3, -0.4, -0.5, -0.6, -0.7, -0.8, -0.9, -1.0] !! index)
    else return 0


getNewPrice :: IO Float
getNewPrice = do
    var <- getVariation
    if var == 1 then return 0.5
    else if var == (-1) then return (-0.5)
    else return 0


attStockPriceFor :: Int -> IO ()
attStockPriceFor seg = do
    startTime <- getCurrentTime
    let endTime = addUTCTime (fromIntegral seg) startTime
    loop endTime

loop :: UTCTime -> IO ()
loop endTime = do
    currentTime <- getCurrentTime
    if currentTime >= endTime
        then putStrLn "Tempo esgotado."
        else do
            attStocksPrice
            threadDelay (1 * 500000)
            loop endTime

attStocksPrice :: IO ()
attStocksPrice = do
    newPrice <- getNewPriceOld
    setSaldo 1 newPrice
    updateHBStockPrice "./HomeBroker/homebroker_test.txt" (getSaldo 1)
    printMatrix "./HomeBroker/homebroker_test.txt"

main :: IO ()
main = do
    attStockPriceFor 5

    -- setSaldo 1 0.7
    -- putStrLn $ show (getSaldo 1)


