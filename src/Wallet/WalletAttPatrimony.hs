module Wallet.WalletAttPatrimony where
import Client.ModelClient (Asset (companyID, qtd), Client (ident))
import Company.GetSetAttrsCompany (getPrice)
import Client.GetSetAttrsClient (getAllAssets, setPatrimony)


getNewPatrimony :: Int -> IO Float
getNewPatrimony idClient = do 
    let newPatri = getNewPatrimonyAux (getAllAssets idClient)
    setPatrimony idClient newPatri
    return newPatri


getNewPatrimonyAux :: [Asset] -> Float
getNewPatrimonyAux [] = 0
getNewPatrimonyAux (x:xs) = do
    let id = companyID x
        qtd_ = qtd x
    format (fromIntegral qtd_ * getPrice id + getNewPatrimonyAux xs)
    where
        format :: Float -> Float
        format newPrice = fromIntegral (round (newPrice * 10 )) / 10


attAllClientsWalletPatrimonyGraph :: [Client] -> IO ()
attAllClientsWalletPatrimonyGraph [] = return ()
attAllClientsWalletPatrimonyGraph (x:xs) = do
    getNewPatrimony (ident x)
    attAllClientsWalletPatrimonyGraph xs
