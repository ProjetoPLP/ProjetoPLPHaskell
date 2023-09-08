module HomeBroker.HomeBrokerGraphUpdate where

import Control.Concurrent (threadDelay)
import System.Random (randomRIO)
import Data.Time.Clock (getCurrentTime, UTCTime, addUTCTime)

import HomeBroker.HomeBrokerUpdate
import Utils.MatrixUtils (printMatrix, writeMatrixValue)

import Company.GetSetAttrsCompany
import Clock.ClockUpdate
import Clock.GetSetClock (addClock, getClock)


updateHBGraphCandle :: FilePath -> Int -> Int -> IO ()
updateHBGraphCandle filePath row col = do
    writeMatrixValue filePath "❚" row col


checkNewHBCandle :: Int -> Float -> Float ->  IO ()
checkNewHBCandle id oldPrice newPrice = do
    if newPrice > oldPrice then do
        if getRow id == 6 then do
            cleanHBGraph ("./Company/HomeBroker/homebroker" ++ show id ++ ".txt") 6
            updateRow id 20
        else
            updateRow id (-1)
    
    else if newPrice < oldPrice then do
        if getRow id == 26 then do
            cleanHBGraph ("./Company/HomeBroker/homebroker" ++ show id ++ ".txt") 6
            updateRow id (-20)
        else
            updateRow id 1

    else updateRow id 0

-- Reinicia o gráfico sobrescrevendo todos os espaços com caracteres vazios
cleanHBGraph :: FilePath -> Int -> IO ()
cleanHBGraph filepath 26 = writeMatrixValue filepath (replicate 74 ' ') 26 2
cleanHBGraph filepath row = do
    writeMatrixValue filepath (replicate 74 ' ') row 2
    cleanHBGraph filepath (row + 1)


-- Retorna um index e uma variação aleatória 
getIndexAndVariation :: IO [Int]
getIndexAndVariation = do
    index <- randomRIO (0,9 :: Int)
    var <- randomRIO (-1,1 :: Int)
    return [index, var]


-- Retorna um novo preço baseado na aleatóriedade da função getIndexAndVariation
getNewPrice :: Float -> IO Float
getNewPrice oldPrice = do
    indexVar <- getIndexAndVariation
    if last indexVar == 1 then return (format (([0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0] !! head indexVar) + oldPrice))
    else if last indexVar == (-1) then return (format (([-0.1, -0.2, -0.3, -0.4, -0.5, -0.6, -0.7, -0.8, -0.9, -1.0] !! head indexVar) + oldPrice))
    else return oldPrice
    where 
        format :: Float -> Float
        format newPrice = fromIntegral (round (newPrice * 10 )) / 10


attStockPriceFor :: Int -> Int -> IO ()
attStockPriceFor id seg = do
    startTime <- getCurrentTime
    addClock seg
    newClockHour <- getClock
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
    let oldPrice = getPrice id
    newPrice <- getNewPrice oldPrice

    setPrice id newPrice
    updateHBStockPrice ("./Company/HomeBroker/homebroker" ++ show id ++ ".txt") oldPrice newPrice
    updateHBGraphCandle ("./Company/HomeBroker/homebroker" ++ show id ++ ".txt") (getRow id) (getCol id)
    printMatrix ("./Company/HomeBroker/homebroker" ++ show id ++ ".txt")

