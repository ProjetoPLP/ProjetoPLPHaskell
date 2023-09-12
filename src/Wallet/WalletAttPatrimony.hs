module Wallet.WalletAttPatrimony where

import Client.ModelClient (Asset (companyID, qtd), Client (ident))
import Company.GetSetAttrsCompany (getPrice)
import Client.GetSetAttrsClient (getAllAssets, setPatrimony, getRow, getCol, getPatrimony)
import Utils.MatrixUtils (writeMatrixValue)
import Utils.GraphUtils (updateWalletGraphCandle, attClientLineRow)


-- Atualiza o patrimônio de um cliente
attClientPatrimony :: Int -> IO ()
attClientPatrimony idClient = do 
    setPatrimony idClient (attClientPatrimonyAux (getAllAssets idClient))


getOldPatrimony :: Int -> IO Float
getOldPatrimony idUser = do
    return (getPatrimony idUser)

attClientPatrimonyAux :: [Asset] -> Float
attClientPatrimonyAux [] = 0
attClientPatrimonyAux (x:xs) = do
    let id = companyID x
        qtd_ = qtd x
    format (fromIntegral qtd_ * getPrice id + attClientPatrimonyAux xs)
    where
        format :: Float -> Float
        format newPrice = fromIntegral (round (newPrice * 10 )) / 10


attAllClientsWalletPatrimonyGraph :: [Client] -> IO ()
attAllClientsWalletPatrimonyGraph [] = return ()
attAllClientsWalletPatrimonyGraph (x:xs) = do
    let oldPatrimony = getPatrimony (ident x)
    let newPatrimony = attClientPatrimonyAux (getAllAssets (ident x))
    attClientWalletPatrimonyGraph (ident x) oldPatrimony newPatrimony
    attClientPatrimony (ident x)
    attAllClientsWalletPatrimonyGraph xs


attClientWalletPatrimonyGraph :: Int -> Float -> Float -> IO ()
attClientWalletPatrimonyGraph idClient oldPatrimony newPatrimony = do
    attClientLineRow idClient oldPatrimony newPatrimony
    updateWalletGraphCandle ("./Client/Wallet/wallet" ++ show idClient ++ ".txt") (getRow idClient) (getCol idClient)
    
