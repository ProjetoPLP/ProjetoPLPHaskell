module Menus.HomeBroker.BuySell.HomeBrokerBuySellLogic where

import Models.Client.GetSetAttrsClient ( getQtdAssetsInCompany, getCash, addCash )
import Models.Client.PostClient ( addAsset )
import Models.Company.GetSetAttrsCompany ( getPrice )


-- Compra X ações para um cliente de uma determinada empresa
buy :: Int -> Int -> Int -> IO ()
buy idClient idComp num = do
    if num <= 0 then return ()
    else do
        let cash = getCash idClient
            totalPrice = getPrice idComp * fromIntegral num

        if totalPrice > cash then return ()
        else do
            addCash idClient (-totalPrice)
            addAsset idClient idComp num


-- Vende X ações de um cliente de uma determinada empresa
sell :: Int -> Int -> Int -> IO ()
sell idClient idComp num = do
    if num <= 0 then return ()
    else do
        let cash = getCash idClient
            totalPrice = getPrice idComp * fromIntegral num

        if num > getQtdAssetsInCompany idClient idComp then return ()
        else do
            addCash idClient totalPrice
            addAsset idClient idComp (-num)