module HomeBroker.HomeBrokerLoopLogic where

import Control.Concurrent (threadDelay)
import Data.Time.Clock (getCurrentTime, UTCTime, addUTCTime)

import Clock.ClockUpdate
import Clock.GetSetClock (addClock, getClock)

import Company.GetSetAttrsCompany
import Company.SaveCompany (getCompanyJSON)
import HomeBroker.HomeBrokerAttPrice (attCompanyPriceGraph, attAllCompanyPrice)
import Utils.GraphUtils (checkCompanyColumn, checkAllCompanyColumn)


-- Define, a partir da entrada do usuário, por quanto tempo o preço e o gráfico deve variar
callLoop :: Int -> Int -> IO ()
callLoop id seg = do
    startTime <- getCurrentTime
    addClock seg
    updateMatrixClock ("./Company/HomeBroker/homebroker" ++ show id ++ ".txt") getClock
    let endTime = addUTCTime (fromIntegral seg) startTime
    loop id endTime


-- Atualiza o preço e o gráfico em TODAS as empresas
loop :: Int -> UTCTime -> IO ()
loop id endTime = do
    currentTime <- getCurrentTime
    if currentTime >= endTime then do
        checkAllCompanyColumn (getCompanyJSON "./Data/Companies.json")
        putStrLn "Tempo esgotado."
        else do
            attCompanyPriceGraph id
            attAllCompanyPrice id (getCompanyJSON "./Data/Companies.json")
            threadDelay (1 * 500000)
            loop id endTime