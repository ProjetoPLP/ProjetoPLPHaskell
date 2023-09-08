module HomeBroker.HomeBrokerAttPrice where

import System.Random (randomRIO)

import HomeBroker.HomeBrokerUpdate
import Utils.MatrixUtils (printMatrix, writeMatrixValue)

import Company.GetSetAttrsCompany
import Utils.GraphUtils (attCompanyLineRow, attTrendIndicator)


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


-- Atualiza em uma empresa, a partir do seu ID, o preço e o gráfico
attCompanyPriceGraph :: Int -> IO ()
attCompanyPriceGraph id = do
    let oldPrice = getPrice id
    newPrice <- getNewPrice oldPrice

    setPrice id newPrice
    attTrendIndicator id oldPrice newPrice
    attCompanyLineRow id oldPrice newPrice
    updateHBStockPrice ("./Company/HomeBroker/homebroker" ++ show id ++ ".txt") newPrice (getTrendIndicator id)
    updateHBGraphCandle ("./Company/HomeBroker/homebroker" ++ show id ++ ".txt") (getRow id) (getCol id)
    printMatrix ("./Company/HomeBroker/homebroker" ++ show id ++ ".txt")

