module HomeBroker.HomeBrokerAttPrice where

import System.Random (randomRIO)

import HomeBroker.HomeBrokerUpdate
import Utils.MatrixUtils (printMatrix, writeMatrixValue)

import Company.GetSetAttrsCompany
import Utils.GraphUtils (attCompanyLineRow, attTrendIndicator)
import Company.ModelCompany (Company)
import Text.Read (Lexeme(Ident))


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


-- Atualiza na empresa atual, a partir do seu ID, o preço e o gráfico
attCurrentCompanyPriceGraph :: Int -> IO ()
attCurrentCompanyPriceGraph id = do
    let oldPrice = getPrice id
    newPrice <- getNewPrice oldPrice

    setPrice id newPrice
    attTrendIndicator id oldPrice newPrice
    attCompanyLineRow id oldPrice newPrice
    updateHBStockPrice path newPrice (getTrendIndicator id)
    updateHBGraphCandle path (getRow id) (getCol id)
    printMatrix path
    where path = "./Company/HomeBroker/homebroker" ++ show id ++ ".txt"


-- Atualiza em uma empresa qualquer, a partir do seu ID, o preço e o gráfico
attOthersCompanyPriceGraph :: Int -> IO ()
attOthersCompanyPriceGraph id = do
    let oldPrice = getPrice id
    newPrice <- getNewPrice oldPrice

    setPrice id newPrice
    attTrendIndicator id oldPrice newPrice
    attCompanyLineRow id oldPrice newPrice
    updateHBStockPrice path newPrice (getTrendIndicator id)
    updateHBGraphCandle path (getRow id) (getCol id)
    where path = "./Company/HomeBroker/homebroker" ++ show id ++ ".txt"


-- Atualiza o preço e o gráfico em todas as empresas cadastradas
attAllCompanyPriceGraph :: Int -> [Company] -> IO ()
attAllCompanyPriceGraph _ [] = return ()
attAllCompanyPriceGraph id (x:xs) = do
    if getIdent x == id then do
        attAllCompanyPriceGraph id xs
    else do
        attOthersCompanyPriceGraph (getIdent x)
        attAllCompanyPriceGraph id xs