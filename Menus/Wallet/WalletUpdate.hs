module Menus.Wallet.WalletUpdate where

import Utils.MatrixUtils ( writeMatrixValue )
import Utils.UpdateUtils ( fillLeft, resetMenu )
import Models.Client.GetSetAttrsClient as Cli ( getCPF, getCash, getName, getPatrimony, getAllAssets )
import Models.Client.ModelClient ( Asset (companyID, qtd) )
import Models.Company.GetSetAttrsCompany as Com ( getCode, getPrice, getTrendIndicator )
import Models.Clock.ClockUpdate ( updateMatrixClock )
import Menus.Wallet.WalletAttPatrimony ( attClientPatrimony )


-- Aualiza todas as informações da carteira do cliente
updateClientWallet :: Int -> IO ()
updateClientWallet idUser = do
    resetStocks [1..12] idUser
    updateMatrixClock filePath
    updateWLCash filePath (getCash idUser)
    attClientPatrimony idUser
    updateWLPatrimony filePath (getPatrimony idUser)
    updateWLUserName filePath (Cli.getName idUser)
    updateWLUserCPF filePath (getCPF idUser)
    updateAllWLCompanyCode filePath ownedAssets
    updateAllWLCompanyPrice filePath ownedAssets
    updateAllWLOwnedStocks filePath ownedAssets
    where
        filePath = "./Models/Client/Wallets/wallet" ++ show idUser ++ ".txt"
        ownedAssets = getAllAssets idUser


-- Aualiza todas as informações do menu de depósito
updateWalletDeposito :: Int -> IO ()
updateWalletDeposito idUser = do
    resetMenu filePath "./Sprites/Wallet/walletDeposito_base.txt"
    updateMatrixClock filePath
    updateWLCash filePath (getCash idUser)
    updateWLPatrimony filePath (getPatrimony idUser)
    updateWLUserName filePath (Cli.getName idUser)
    updateWLUserCPF filePath (getCPF idUser)
    where
        filePath = "./Menus/Wallet/DepositoSaque/walletDeposito.txt"


-- Aualiza todas as informações do menu de saque
updateWalletSaque :: Int -> IO ()
updateWalletSaque idUser = do
    resetMenu filePath "./Sprites/Wallet/walletSaque_base.txt"
    updateMatrixClock filePath
    updateWLCash filePath (getCash idUser)
    updateWLPatrimony filePath (getPatrimony idUser)
    updateWLUserName filePath (Cli.getName idUser)
    updateWLUserCPF filePath (getCPF idUser)
    where
        filePath = "./Menus/Wallet/DepositoSaque/walletSaque.txt"


updateWLCash :: FilePath -> Float -> IO ()
updateWLCash filePath cash = do
    let val = fillLeft (show cash ++ "0") 9
    writeMatrixValue filePath val 13 (22 - length val)


updateWLPatrimony :: FilePath -> Float -> IO ()
updateWLPatrimony filePath patri = do
    let val = fillLeft (show patri ++ "0") 10
    writeMatrixValue filePath val 6 (24 - length val)


-- Atualiza o código de todas as empresas no carteira do usuário
updateAllWLCompanyCode :: FilePath -> [Asset] -> IO ()
updateAllWLCompanyCode filePath [] = return ()
updateAllWLCompanyCode filePath (x:xs) = do
    let id = companyID x
    updateWLCompanyCode filePath id (getCode id)
    updateAllWLCompanyCode filePath xs


updateWLCompanyCode :: FilePath -> Int -> String -> IO ()
updateWLCompanyCode filePath id code = do
    let pos = getCompanyCodePosition id
    writeMatrixValue filePath code (head pos) (last pos)


-- Atualiza o preço de todas as empresas no carteira do usuário
updateAllWLCompanyPrice :: FilePath -> [Asset] -> IO ()
updateAllWLCompanyPrice filePath [] = return ()
updateAllWLCompanyPrice filePath (x:xs) = do
    let id = companyID x
    updateWLCompanyPrice filePath id (show (getPrice id) ++ "0") (getTrendIndicator id)
    updateAllWLCompanyPrice filePath xs


updateWLCompanyPrice :: FilePath -> Int -> String -> String -> IO ()
updateWLCompanyPrice filePath id price trendInd = do
    let pos = getCompanyPricePosition id
        val = fillLeft (trendInd ++ price) 7
    writeMatrixValue filePath val (head pos) (last pos - length val)


-- Atualiza a quantidade de ações que o usuário possui de cada empresa na sua carteira
updateAllWLOwnedStocks :: FilePath -> [Asset] -> IO ()
updateAllWLOwnedStocks filePath [] = return ()
updateAllWLOwnedStocks filePath (x:xs) = do
    let id = companyID x
        qtd_ = qtd x
    updateWLOwnedStocks filePath id (show qtd_)
    updateAllWLOwnedStocks filePath xs


updateWLOwnedStocks :: FilePath -> Int -> String -> IO ()
updateWLOwnedStocks filePath id num = do
    let pos = getOwnedStocksPosition id
        val = fillLeft num 5
    writeMatrixValue filePath val (head pos) (last pos - length val)


updateWLUserName :: FilePath -> String -> IO ()
updateWLUserName filePath name = do
    writeMatrixValue filePath name 10 6


updateWLUserCPF :: FilePath -> String -> IO ()
updateWLUserCPF filePath cpf = do
    writeMatrixValue filePath cpf 11 6


-- Reseta todas as informações de ações do usuário
resetStocks :: [Int] -> Int -> IO ()
resetStocks [] idUser = return ()
resetStocks (x:xs) idUser = do
    updateWLCompanyCode filePath x "-----"
    updateWLCompanyPrice filePath x "     " " "
    updateWLOwnedStocks filePath x "-----"
    resetStocks xs idUser
    where
        filePath = "./Models/Client/Wallets/wallet" ++ show idUser ++ ".txt"


getCompanyCodePosition :: Int -> [Int]
getCompanyCodePosition id
    | id == 1 = [22, 3]
    | id == 2 = [24, 3]
    | id == 3 = [26, 3]
    | id == 4 = [22, 27]
    | id == 5 = [24, 27]
    | id == 6 = [26, 27]
    | id == 7 = [22, 51]
    | id == 8 = [24, 51]
    | id == 9 = [26, 51]
    | id == 10 = [22, 75]
    | id == 11 = [24, 75]
    | id == 12 = [26, 75]


getCompanyPricePosition :: Int -> [Int]
getCompanyPricePosition id
    | id == 1 = [22, 16]
    | id == 2 = [24, 16]
    | id == 3 = [26, 16]
    | id == 4 = [22, 40]
    | id == 5 = [24, 40]
    | id == 6 = [26, 40]
    | id == 7 = [22, 64]
    | id == 8 = [24, 64]
    | id == 9 = [26, 64]
    | id == 10 = [22, 88]
    | id == 11 = [24, 88]
    | id == 12 = [26, 88]


getOwnedStocksPosition :: Int -> [Int]
getOwnedStocksPosition id
    | id == 1 = [22, 23]
    | id == 2 = [24, 23]
    | id == 3 = [26, 23]
    | id == 4 = [22, 47]
    | id == 5 = [24, 47]
    | id == 6 = [26, 47]
    | id == 7 = [22, 71]
    | id == 8 = [24, 71]
    | id == 9 = [26, 71]
    | id == 10 = [22, 95]
    | id == 11 = [24, 95]
    | id == 12 = [26, 95]