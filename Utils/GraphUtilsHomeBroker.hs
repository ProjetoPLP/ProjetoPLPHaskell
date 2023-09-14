module Utils.GraphUtilsHomeBroker where

import Utils.MatrixUtils ( writeMatrixValue )
import Models.Company.GetSetAttrsCompany ( getCol, getIdent, getRow, updateCol, updateRow )
import Models.Company.ModelCompany ( Company )


-- Atualiza em uma empresa, a partir do seu ID, a nova linha e coluna baseado no novo preço
attCompanyLineRow :: Int -> Float -> Float -> IO ()
attCompanyLineRow idComp oldPrice newPrice = do
    checkCompanyColumn idComp
    if newPrice > oldPrice then do
        updateRow idComp (-1)
        checkCompanyRowOverflow idComp
    else if newPrice < oldPrice then do
        updateRow idComp 1
        checkCompanyRowUnderflow idComp
    else updateRow idComp 0


-- Verifica se a coluna do gráfico chegou no limite
checkCompanyColumn :: Int -> IO ()
checkCompanyColumn idComp
    | getCol idComp > 74 = do
        cleanHBGraph ("./Models/Company/HomeBrokers/homebroker" ++ show idComp ++ ".txt") 6
        updateCol idComp (-72)
    | otherwise = 
        updateCol idComp 0


-- Verifica se a linha do gráfico chegou no limite superior
checkCompanyRowOverflow :: Int -> IO ()
checkCompanyRowOverflow idComp
    | getRow idComp < 6 = do
        cleanHBGraph ("./Models/Company/HomeBrokers/homebroker" ++ show idComp ++ ".txt") 6
        updateRow idComp 21
    | otherwise = 
        updateRow idComp 0


-- Verifica se a linha do gráfico chegou no limite inferior
checkCompanyRowUnderflow :: Int -> IO ()
checkCompanyRowUnderflow idComp
    | getRow idComp > 26 = do
        cleanHBGraph ("./Models/Company/HomeBrokers/homebroker" ++ show idComp ++ ".txt") 6
        updateRow idComp (-21)
    | otherwise =
        updateRow idComp 0


-- Atualiza a próxima coluna em todos os gráficos
attAllCompanyColumn :: [Company] -> IO ()
attAllCompanyColumn [] = return ()
attAllCompanyColumn (x:xs) = do
    updateCol (getIdent x) 3
    attAllCompanyColumn xs


-- Reinicia o gráfico do Home Broker sobrescrevendo todos os espaços com caracteres vazios
cleanHBGraph :: FilePath -> Int -> IO ()
cleanHBGraph filepath 26 = writeMatrixValue filepath (replicate 74 ' ') 26 2
cleanHBGraph filepath row = do
    writeMatrixValue filepath (replicate 74 ' ') row 2
    cleanHBGraph filepath (row + 1)