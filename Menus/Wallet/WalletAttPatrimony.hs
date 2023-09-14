module Menus.Wallet.WalletAttPatrimony where

import Utils.GraphUtilsWallet ( updateWalletGraphCandle, attClientLineRow )
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
    where
        format :: Float -> Float
        format newPrice = fromIntegral (round (newPrice * 10 )) / 10


-- Atualiza o gráfico da carteira de todos os clientes
attAllClientsPatrimonyGraph :: [Client] -> IO ()
attAllClientsPatrimonyGraph [] = return ()
attAllClientsPatrimonyGraph (x:xs) = do
    let oldPatrimony = getPatrimony (ident x)
        newPatrimony = attClientPatrimonyAux (getAllAssets (ident x))
    attClientPatrimonyGraph (ident x) oldPatrimony newPatrimony
    attClientPatrimony (ident x)
    attAllClientsPatrimonyGraph xs


attClientPatrimonyGraph :: Int -> Float -> Float -> IO ()
attClientPatrimonyGraph idClient oldPatrimony newPatrimony = do
    attClientLineRow idClient oldPatrimony newPatrimony
    updateWalletGraphCandle filePath (getRow idClient) (getCol idClient)
    where
        filePath = "./Models/Client/Wallets/wallet" ++ show idClient ++ ".txt"