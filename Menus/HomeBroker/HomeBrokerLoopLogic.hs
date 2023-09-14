module Menus.HomeBroker.HomeBrokerLoopLogic where

import Control.Concurrent (threadDelay)
import Data.Time.Clock (getCurrentTime, UTCTime, addUTCTime)

import Models.Clock.ClockUpdate (updateMatrixClock)
import Models.Clock.GetSetClock (addClock)

import Utils.GraphUtilsHomeBroker (attAllCompanyColumn)
import Utils.GraphUtilsWallet (attAllClientColumn)
import Models.Company.SaveCompany (getCompanyJSON)
import Menus.HomeBroker.HomeBrokerAttPrice (attAllCompanyPriceGraph)
import Menus.Wallet.WalletAttPatrimony (attAllClientsWalletPatrimonyGraph)
import Models.Client.SaveClient (getClientJSON)


-- Define, a partir da entrada do usuário, por quanto tempo o preço e o gráfico deve variar
callLoop :: Int -> Int -> IO ()
callLoop idComp seg = do
    startTime <- getCurrentTime
    addClock seg
    updateMatrixClock ("./Models/Company/HomeBrokers/homebroker" ++ show idComp ++ ".txt")
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