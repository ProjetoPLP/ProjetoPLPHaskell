module Utils.HomeBrokerGraphUtils where

import Company.GetSetAttrsCompany as Com (getCol, getIdent, getRow, setTrendIndicator, updateCol, updateRow)
import Utils.MatrixUtils (writeMatrixValue)
import Company.ModelCompany (Company)


-- Atualiza em uma empresa, a partir do seu ID, a nova linha e coluna baseado no novo preço
attCompanyLineRow :: Int -> Float -> Float -> IO ()
attCompanyLineRow idComp oldPrice newPrice = do
    checkCompanyColumn idComp

    if newPrice > oldPrice then do
        Com.updateRow idComp (-1)
        checkCompanyRowOverflow idComp

    else if newPrice < oldPrice then do
        Com.updateRow idComp 1
        checkCompanyRowUnderflow idComp

    else Com.updateRow idComp 0


checkCompanyColumn :: Int -> IO ()
checkCompanyColumn idComp
    | Com.getCol idComp > 74 = do
        cleanHBGraph ("./Company/HomeBroker/homebroker" ++ show idComp ++ ".txt") 6
        Com.updateCol idComp (-72)
    | otherwise = 
        Com.updateCol idComp 0


checkCompanyRowOverflow :: Int -> IO ()
checkCompanyRowOverflow idComp
    | Com.getRow idComp < 6 = do
        cleanHBGraph ("./Company/HomeBroker/homebroker" ++ show idComp ++ ".txt") 6
        Com.updateRow idComp 21
    | otherwise = 
        Com.updateRow idComp 0


checkCompanyRowUnderflow :: Int -> IO ()
checkCompanyRowUnderflow idComp
    | Com.getRow idComp > 26 = do
        cleanHBGraph ("./Company/HomeBroker/homebroker" ++ show idComp ++ ".txt") 6
        Com.updateRow idComp (-21)
    | otherwise =
        Com.updateRow idComp 0


attAllCompanyColumn :: [Company] -> IO ()
attAllCompanyColumn [] = return ()
attAllCompanyColumn (x:xs) = do
    Com.updateCol (getIdent x) 3
    attAllCompanyColumn xs


-- Reinicia o gráfico do Home Broker sobrescrevendo todos os espaços com caracteres vazios
cleanHBGraph :: FilePath -> Int -> IO ()
cleanHBGraph filepath 26 = writeMatrixValue filepath (replicate 74 ' ') 26 2
cleanHBGraph filepath row = do
    writeMatrixValue filepath (replicate 74 ' ') row 2
    cleanHBGraph filepath (row + 1)


-- Atualiza em uma empresa, a partir do seu ID, o novo trendIndicator
attTrendIndicator :: Int -> Float -> Float -> IO ()
attTrendIndicator idComp oldPrice newPrice
    | newPrice > oldPrice = setTrendIndicator idComp "▲"
    | newPrice < oldPrice = setTrendIndicator idComp "▼"
    | otherwise = setTrendIndicator idComp " "