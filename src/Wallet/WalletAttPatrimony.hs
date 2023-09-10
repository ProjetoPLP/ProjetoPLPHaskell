module Wallet.WalletAttPatrimony where
import Client.ModelClient (Asset (companyID, qtd), Client (ident))
import Company.GetSetAttrsCompany (getPrice)
import Client.GetSetAttrsClient (getAllAssets, setPatrimony)


-- Atualiza o patrimônio de um cliente
attClientPatrimony :: Int -> IO ()
attClientPatrimony idClient = do 
    setPatrimony idClient (attClientPatrimonyAux (getAllAssets idClient))


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
    attClientPatrimony (ident x)
    --
    -- atualizar gráfico
    --
    attAllClientsWalletPatrimonyGraph xs
