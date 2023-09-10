module HomeBroker.HomeBrokerLoopLogic where

import Control.Concurrent (threadDelay)
import Data.Time.Clock (getCurrentTime, UTCTime, addUTCTime)

import Clock.ClockUpdate
import Clock.GetSetClock (addClock)

import Company.GetSetAttrsCompany
import Company.SaveCompany (getCompanyJSON)
import HomeBroker.HomeBrokerAttPrice (attCurrentCompanyPriceGraph, attAllCompanyPriceGraph)
import Utils.GraphUtils (checkCompanyColumn, checkAllCompanyColumn)
import Wallet.WalletAttPatrimony ( attAllClientsWalletPatrimonyGraph )
import Client.SaveClient (getClientJSON)


-- Define, a partir da entrada do usuário, por quanto tempo o preço e o gráfico deve variar
callLoop :: Int -> Int -> IO ()
callLoop id seg = do
    startTime <- getCurrentTime
    addClock seg
    updateMatrixClock ("./Company/HomeBroker/homebroker" ++ show id ++ ".txt")
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
            attCurrentCompanyPriceGraph id
            attAllCompanyPriceGraph id (getCompanyJSON "./Data/Companies.json")
            attAllClientsWalletPatrimonyGraph (getClientJSON "./Data/Clients.json")
            threadDelay (1 * 500000)
            loop id endTime