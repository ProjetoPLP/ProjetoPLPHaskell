module HomeBroker.HomeBrokerLoopLogic where

import Control.Concurrent (threadDelay)
import Data.Time.Clock (getCurrentTime, UTCTime, addUTCTime)

import Clock.ClockUpdate
import Clock.GetSetClock (addClock, getClock)

import Company.GetSetAttrsCompany
import HomeBroker.HomeBrokerAttPrice (attCompanyPriceGraph)
import Utils.GraphUtils (checkCompanyColumn)


-- Define, a partir da entrada do usuário, por quanto tempo o preço e o gráfico deve variar
callLoop :: Int -> Int -> IO ()
callLoop id seg = do
    startTime <- getCurrentTime
    addClock seg
    newClockHour <- getClock
    updateMatrixClock ("./Company/HomeBroker/homebroker" ++ show id ++ ".txt") newClockHour
    let endTime = addUTCTime (fromIntegral seg) startTime
    loop id endTime


-- Atualiza o preço e o gráfico em TODAS as empresas
loop :: Int -> UTCTime -> IO ()
loop id endTime = do
    currentTime <- getCurrentTime
    if currentTime >= endTime then do
        checkCompanyColumn id
        putStrLn "Tempo esgotado."
    else do
        attCompanyPriceGraph id
        threadDelay (1 * 500000)
        loop id endTime