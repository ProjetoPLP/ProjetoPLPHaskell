module Menus.HomeBroker.HomeBrokerAttPrice where

import System.Random ( randomRIO )
import Utils.MatrixUtils ( printMatrix )
import Utils.UpdateUtils ( format )
import Utils.GraphUtilsHomeBroker ( attCompanyLineRow, updateHBGraphCandle )
import Menus.HomeBroker.HomeBrokerUpdate ( updateHBStockMaxPrice, updateHBStockMinPrice, updateHBStockPrice, updateHBStockStartPrice )
import Menus.HomeBroker.CompanyDown.CompanyDownUpdate ( isDown, removeComapanyFromExchange )
import Models.Company.ModelCompany ( Company )
import Models.Company.GetSetAttrsCompany ( getCol, getIdent, getMaxPrice, getMinPrice, getPrice, getRow, getStartPrice, getTrendIndicator, setMaxPrice, setMinPrice, setPrice, setTrendIndicator )


-- Retorna um novo preço
getNewPrice :: Float -> IO Float
getNewPrice oldPrice = do
    -- newPrice <- randomRIO (0.1, 1.0 :: Float)
    -- var <- randomRIO (-1,1 :: Int)
    -- if var == 1 then return (format (newPrice + oldPrice))
    -- else if var == (-1) then return (format (-newPrice + oldPrice))
    -- else return oldPrice
    newPrice <- randomRIO (-1.0, 1.0 :: Float)
    return (format (newPrice + oldPrice))


-- Atualiza em uma empresa, a partir do seu ID, o novo trendIndicator
attCompanyTrendIndicator :: Int -> Float -> Float -> IO ()
attCompanyTrendIndicator idComp oldPrice newPrice
    | newPrice > oldPrice = setTrendIndicator idComp "▲"
    | newPrice < oldPrice = setTrendIndicator idComp "▼"
    | otherwise = setTrendIndicator idComp " "


-- Retorna o novo preço máximo baseado no novo preço
getNewMaxPrice :: Int -> Float -> IO Float
getNewMaxPrice idComp newPrice
    | maxPrice >= newPrice = return maxPrice 
    | otherwise = do
        setMaxPrice idComp newPrice 
        return newPrice
    where
        maxPrice = getMaxPrice idComp


-- Retorna o novo preço mínimo baseado no novo preço
getNewMinPrice :: Int -> Float -> IO Float
getNewMinPrice idComp newPrice
    | minPrice <= newPrice = return minPrice 
    | otherwise = do
        setMinPrice idComp newPrice 
        return newPrice
    where
        minPrice = getMinPrice idComp


-- Atualiza o preço e o gráfico de todas as empresas
attAllCompanyPriceGraph :: Int -> [Company] -> IO ()
attAllCompanyPriceGraph _ [] = return ()
attAllCompanyPriceGraph idComp (x:xs)
    | randomId == idComp = do
        attCurrentCompanyPriceGraph idComp
        attAllCompanyPriceGraph idComp xs
    | isDown randomId = do
        removeComapanyFromExchange randomId
        attAllCompanyPriceGraph idComp xs
    | otherwise = do
        attCompanyPriceGraph randomId
        attAllCompanyPriceGraph idComp xs
    where
        randomId = getIdent x


-- Atualiza o preço e o gráfico na empresa que está sendo exibida
attCurrentCompanyPriceGraph :: Int -> IO ()
attCurrentCompanyPriceGraph id = do
    attCompanyPriceGraph id
    printMatrix ("./Models/Company/HomeBrokers/homebroker" ++ show id ++ ".txt")


-- Atualiza em uma empresa qualquer, a partir do seu ID, o preço e o gráfico
attCompanyPriceGraph :: Int -> IO ()
attCompanyPriceGraph id = do
    let oldPrice = getPrice id
    newPrice <- getNewPrice oldPrice
    newMaxPrice <- getNewMaxPrice id newPrice
    newMinPrice <- getNewMinPrice id newPrice

    setPrice id newPrice
    attCompanyTrendIndicator id oldPrice newPrice
    attCompanyLineRow id oldPrice newPrice
    updateHBStockPrice filePath newPrice (getTrendIndicator id)
    updateHBStockMaxPrice filePath newMaxPrice
    updateHBStockMinPrice filePath newMinPrice
    updateHBStockStartPrice filePath (getStartPrice id)
    updateHBGraphCandle filePath (getRow id) (getCol id)
    where
        filePath = "./Models/Company/HomeBrokers/homebroker" ++ show id ++ ".txt"