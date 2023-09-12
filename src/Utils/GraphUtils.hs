module Utils.GraphUtils where
import Company.GetSetAttrsCompany as Com (getCol, getIdent, getRow, setTrendIndicator, updateCol, updateRow)
import Client.GetSetAttrsClient as Cli (getCol, getRow, updateCol, updateRow)
import Utils.MatrixUtils (writeMatrixValue)
import HomeBroker.HomeBrokerUpdate (cleanHBGraph)
import Company.ModelCompany (Company)
import Client.ModelClient


-- Atualiza em uma empresa, a partir do seu ID, a nova linha e coluna baseado no novo preço
attCompanyLineRow :: Int -> Float -> Float -> IO ()
attCompanyLineRow id oldPrice newPrice = do
    if newPrice > oldPrice then do
        checkCompanyRowOverflow id

    else if newPrice < oldPrice then do
        checkCompanyRowUnderflow id

    else Com.updateRow id 0


checkCompanyColumn :: Int -> IO ()
checkCompanyColumn id = do
    if Com.getCol id == 72 then do
        cleanHBGraph ("./Company/HomeBroker/homebroker" ++ show id ++ ".txt") 6
        Com.updateCol id (-69)
    else
        Com.updateCol id 3


checkAllCompanyColumn :: [Company] -> IO()
checkAllCompanyColumn [] = return ()
checkAllCompanyColumn (x:xs) = do
    checkCompanyColumn (getIdent x)
    checkAllCompanyColumn xs


checkCompanyRowOverflow :: Int -> IO ()
checkCompanyRowOverflow id = do
    if Com.getRow id == 6 then do
            cleanHBGraph ("./Company/HomeBroker/homebroker" ++ show id ++ ".txt") 6
            Com.updateRow id 20
        else
            Com.updateRow id (-1)


checkCompanyRowUnderflow :: Int -> IO ()
checkCompanyRowUnderflow id = do
    if Com.getRow id == 26 then do
            cleanHBGraph ("./Company/HomeBroker/homebroker" ++ show id ++ ".txt") 6
            Com.updateRow id (-20)
        else
            Com.updateRow id 1


-- Atualiza em uma empresa, a partir do seu ID, o novo trendIndicator
attTrendIndicator :: Int -> Float -> Float -> IO ()
attTrendIndicator id oldPrice newPrice = do
    if newPrice > oldPrice then do
        setTrendIndicator id "▲"
    else if newPrice < oldPrice then do
        setTrendIndicator id "▼"
    else
        setTrendIndicator id " "


attClientLineRow :: Int -> Float -> Float -> IO ()
attClientLineRow id oldPatrimony newPatrimony = do
    if newPatrimony > oldPatrimony then do
        checkClientRowOverflow id

    else if newPatrimony < oldPatrimony then do
        checkClientRowUnderflow id

    else Cli.updateRow id 0


checkClientRowOverflow :: Int -> IO ()
checkClientRowOverflow id = do
    if Cli.getRow id == 10 then do
            cleanWLGraph ("./Client/Wallet/wallet" ++ show id ++ ".txt") 10
            Cli.updateRow id 10
    else
        Cli.updateRow id (-1)


checkClientRowUnderflow :: Int -> IO ()
checkClientRowUnderflow id = do
    if Cli.getRow id == 20 then do
            cleanWLGraph ("./Client/Wallet/wallet" ++ show id ++ ".txt") 10
            Cli.updateRow id (-10)
        else
            Cli.updateRow id 1


checkClientColumn :: Int -> IO ()
checkClientColumn id = do
    if Cli.getCol id == 96 then do
        cleanWLGraph ("./Client/Wallet/wallet" ++ show id ++ ".txt") 10
        Cli.updateCol id (-45)
        else
            Cli.updateCol id 2


checkAllClientColumn :: [Client] -> IO()
checkAllClientColumn [] = return ()
checkAllClientColumn (x:xs) = do
    checkClientColumn (ident x)
    checkAllClientColumn xs


cleanWLGraph :: FilePath -> Int -> IO ()
cleanWLGraph filepath 20 = writeMatrixValue filepath (replicate 47 ' ') 20 50
cleanWLGraph filepath row = do
    writeMatrixValue filepath (replicate 47 ' ') row 50
    cleanWLGraph filepath (row + 1)


updateWalletGraphCandle :: FilePath -> Int -> Int -> IO ()
updateWalletGraphCandle filePath row col = do
    writeMatrixValue filePath "❚" row col
