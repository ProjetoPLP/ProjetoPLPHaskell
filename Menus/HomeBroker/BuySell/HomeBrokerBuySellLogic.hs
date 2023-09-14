module Menus.HomeBroker.BuySell.HomeBrokerBuySellLogic where

import Client.GetSetAttrsClient (getQtdAssetsInCompany, getCash, removeCash, addCash)
import Client.PostClient (addAsset)
import Company.GetSetAttrsCompany (getPrice)


buy :: Int -> Int -> Int -> IO ()
buy idClient idComp num = do
    if num <= 0 then return () else do
        let cash = getCash idClient
            totalPrice = getPrice idComp * fromIntegral num

        if totalPrice > cash then return ()
        else do
            removeCash idClient totalPrice
            addAsset idClient idComp num


sell :: Int -> Int -> Int -> IO ()
sell idClient idComp num = do
    if num <= 0 then return () else do
        let cash = getCash idClient
            totalPrice = getPrice idComp * fromIntegral num

        if num > getQtdAssetsInCompany idClient idComp then return ()
        else do
            addCash idClient totalPrice
            addAsset idClient idComp (-num)