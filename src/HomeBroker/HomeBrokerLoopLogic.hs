module HomeBroker.HomeBrokerLoopLogic where

import Control.Concurrent (threadDelay)
import Data.Time.Clock (getCurrentTime, UTCTime, addUTCTime)

import Clock.ClockUpdate (updateMatrixClock)
import Clock.GetSetClock (addClock)

import Utils.HomeBrokerGraphUtils (attAllCompanyColumn)
import Utils.WalletGraphUtils (attAllClientColumn)
import Company.SaveCompany (getCompanyJSON)
import HomeBroker.HomeBrokerAttPrice (attAllCompanyPriceGraph)
import Wallet.WalletAttPatrimony (attAllClientsWalletPatrimonyGraph)
import Client.SaveClient (getClientJSON)


-- Define, a partir da entrada do usuário, por quanto tempo o preço e o gráfico deve variar
callLoop :: Int -> Int -> IO ()
callLoop idComp seg = do
    startTime <- getCurrentTime
    addClock seg
    updateMatrixClock ("./Company/HomeBroker/homebroker" ++ show idComp ++ ".txt")
    let endTime = addUTCTime (fromIntegral seg) startTime
    loop idComp endTime


-- Atualiza o preço e o gráfico em TODAS as empresas
loop :: Int -> UTCTime -> IO ()
loop idComp endTime = do
    currentTime <- getCurrentTime
    if currentTime >= endTime then do
        attAllCompanyColumn (getCompanyJSON "./Data/Companies.json")
        attAllClientColumn (getClientJSON "./Data/Clients.json")
        else do
            attAllCompanyPriceGraph idComp (getCompanyJSON "./Data/Companies.json")
            attAllClientsWalletPatrimonyGraph (getClientJSON "./Data/Clients.json")
            threadDelay 500000
            loop idComp endTime