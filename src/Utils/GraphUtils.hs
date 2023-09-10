module Utils.GraphUtils where
import Company.GetSetAttrsCompany (getCol, getIdent, getRow, setTrendIndicator, updateCol, updateRow)
import Utils.MatrixUtils (writeMatrixValue)
import HomeBroker.HomeBrokerUpdate (cleanHBGraph)
import Company.ModelCompany (Company)


-- Atualiza em uma empresa, a partir do seu ID, a nova linha e coluna baseado no novo preço
attCompanyLineRow :: Int -> Float -> Float -> IO ()
attCompanyLineRow id oldPrice newPrice = do
    if newPrice > oldPrice then do
        checkCompanyRowOverflow id

    else if newPrice < oldPrice then do
        checkCompanyRowUnderflow id

    else updateRow id 0


checkCompanyColumn :: Int -> IO ()
checkCompanyColumn id = do
    if getCol id == 72 then do
        cleanHBGraph ("./Company/HomeBroker/homebroker" ++ show id ++ ".txt") 6
        updateCol id (-69)
    else
        updateCol id 3


checkAllCompanyColumn :: [Company] -> IO()
checkAllCompanyColumn [] = return ()
checkAllCompanyColumn (x:xs) = do
    checkCompanyColumn (getIdent x)
    checkAllCompanyColumn xs


checkCompanyRowOverflow :: Int -> IO ()
checkCompanyRowOverflow id = do
    if getRow id == 6 then do
            cleanHBGraph ("./Company/HomeBroker/homebroker" ++ show id ++ ".txt") 6
            updateRow id 20
        else
            updateRow id (-1)


checkCompanyRowUnderflow :: Int -> IO ()
checkCompanyRowUnderflow id = do
    if getRow id == 26 then do
            cleanHBGraph ("./Company/HomeBroker/homebroker" ++ show id ++ ".txt") 6
            updateRow id (-20)
        else
            updateRow id 1


-- Atualiza em uma empresa, a partir do seu ID, o novo trendIndicator
attTrendIndicator :: Int -> Float -> Float -> IO ()
attTrendIndicator id oldPrice newPrice = do
    if newPrice > oldPrice then do
        setTrendIndicator id "▲"
    else if newPrice < oldPrice then do
        setTrendIndicator id "▼"
    else
        setTrendIndicator id " "