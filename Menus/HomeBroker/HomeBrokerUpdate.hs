module Menus.HomeBroker.HomeBrokerUpdate where

import Utils.MatrixUtils ( writeMatrixValue )
import Utils.UpdateUtils ( fillLeft, resetMenu, getCompanyNameCol )
import Models.Company.GetSetAttrsCompany ( getPrice, getTrendIndicator, getStartPrice, getMaxPrice, getMinPrice, getName, getCode )
import Models.Client.GetSetAttrsClient ( getCash, getQtdAssetsInCompany )
import Models.Clock.ClockUpdate ( updateMatrixClock )


-- Atualiza todas as informações do Home Broker de uma determinada empresa
updateHomeBroker :: Int -> Int -> IO ()
updateHomeBroker idClient idComp = do
    updateMatrixClock filePath
    updateHBCash filePath (getCash idClient)
    updateHBCompanyName filePath (getName idComp)
    updateHBCompanyCode filePath (getCode idComp)
    updateHBStockPrice filePath (getPrice idComp) (getTrendIndicator idComp)
    updateHBStockStartPrice filePath (getStartPrice idComp)
    updateHBStockMaxPrice filePath (getMaxPrice idComp)
    updateHBStockMinPrice filePath (getMinPrice idComp)
    updateHBOwnedStocks filePath (getQtdAssetsInCompany idClient idComp)
    where filePath = "./Models/Company/HomeBrokers/homebroker" ++ show idComp ++ ".txt"


-- Atualiza todas as informações do menu de compras em um Home Broker de uma determinada empresa
updateHomeBrokerBuy :: Int -> Int -> IO ()
updateHomeBrokerBuy idClient idComp = do
    resetMenu filePath "./Sprites/HomeBroker/homebrokerBuy_base.txt"
    updateMatrixClock filePath
    updateHBCash filePath (getCash idClient)
    updateHBCompanyName filePath (getName idComp)
    updateHBCompanyCode filePath (getCode idComp)
    updateHBStockPrice filePath (getPrice idComp) (getTrendIndicator idComp)
    updateHBOwnedStocks filePath (getQtdAssetsInCompany idClient idComp)
    where filePath = "./Menus/HomeBroker/BuySell/homebrokerBuy.txt"


-- Atualiza todas as informações do menu de vendas em um Home Broker de uma determinada empresa
updateHomeBrokerSell :: Int -> Int -> IO ()
updateHomeBrokerSell idClient idComp = do
    resetMenu filePath "./Sprites/HomeBroker/homebrokerSell_base.txt"
    updateMatrixClock filePath
    updateHBCash filePath (getCash idClient)
    updateHBCompanyName filePath (getName idComp)
    updateHBCompanyCode filePath (getCode idComp)
    updateHBStockPrice filePath (getPrice idComp) (getTrendIndicator idComp)
    updateHBOwnedStocks filePath (getQtdAssetsInCompany idClient idComp)
    where filePath = "./Menus/HomeBroker/BuySell/homebrokerSell.txt"


updateHBStockPrice :: FilePath -> Float -> String -> IO ()
updateHBStockPrice filePath price trendInd = do
    let val = fillLeft (trendInd ++ show price ++ "0") 7
    writeMatrixValue filePath val 11 (95 - length val)


updateHBStockMaxPrice :: FilePath -> Float -> IO ()
updateHBStockMaxPrice filePath price = do
    let val = fillLeft (show price ++ "0") 6
    writeMatrixValue filePath val 16 (95 - length val)


updateHBStockMinPrice :: FilePath -> Float -> IO ()
updateHBStockMinPrice filePath price = do
    let val = fillLeft (show price ++ "0") 6
    writeMatrixValue filePath val 18 (95 - length val)


updateHBStockStartPrice :: FilePath -> Float -> IO ()
updateHBStockStartPrice filePath price = do
    let val = fillLeft (show price ++ "0") 6
    writeMatrixValue filePath val 14 (95 - length val)


updateHBCash :: FilePath -> Float -> IO ()
updateHBCash filePath cash = do
    let val = fillLeft (show cash ++ "0") 9
    writeMatrixValue filePath val 3 (77 - length val)


updateHBOwnedStocks :: FilePath -> Int -> IO ()
updateHBOwnedStocks filePath num = do
    let val = fillLeft (show num) 5
    writeMatrixValue filePath val 21 (95 - length val)


updateHBCompanyCode :: FilePath -> String -> IO ()
updateHBCompanyCode filePath name = do
    writeMatrixValue filePath name 3 47


updateHBCompanyName :: FilePath -> String -> IO ()
updateHBCompanyName filePath name = do
    writeMatrixValue filePath name 7 (getCompanyNameCol (length name) 86)