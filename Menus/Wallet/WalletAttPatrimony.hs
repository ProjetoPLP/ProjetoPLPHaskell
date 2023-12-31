module Menus.Wallet.WalletAttPatrimony where

import Utils.UpdateUtils ( format )
import Utils.GraphUtilsWallet ( updateWLGraphCandle, attClientLineRow )
import Models.Client.ModelClient ( Asset (companyID, qtd), Client (ident) )
import Models.Company.GetSetAttrsCompany ( getPrice )
import Models.Client.GetSetAttrsClient ( getAllAssets, setPatrimony, getRow, getCol, getPatrimony )


-- Atualiza o patrimônio de um cliente
attClientPatrimony :: Int -> IO ()
attClientPatrimony idClient = do 
    setPatrimony idClient (attClientPatrimonyAux (getAllAssets idClient))


attClientPatrimonyAux :: [Asset] -> Float
attClientPatrimonyAux [] = 0
attClientPatrimonyAux (x:xs) = do
    let idUser = companyID x
        qtd_ = qtd x
    format (fromIntegral qtd_ * getPrice idUser + attClientPatrimonyAux xs)


-- Atualiza o gráfico da carteira de todos os clientes
attAllClientsPatrimonyGraph :: [Client] -> IO ()
attAllClientsPatrimonyGraph [] = return ()
attAllClientsPatrimonyGraph (x:xs) = do
    let oldPatrimony = getPatrimony idUser
        newPatrimony = attClientPatrimonyAux (getAllAssets idUser)
    attClientPatrimonyGraph idUser oldPatrimony newPatrimony
    attClientPatrimony idUser
    attAllClientsPatrimonyGraph xs
    where
        idUser = ident x


attClientPatrimonyGraph :: Int -> Float -> Float -> IO ()
attClientPatrimonyGraph idClient oldPatrimony newPatrimony = do
    attClientLineRow idClient oldPatrimony newPatrimony
    updateWLGraphCandle filePath (getRow idClient) (getCol idClient)
    where
        filePath = "./Models/Client/Wallets/wallet" ++ show idClient ++ ".txt"