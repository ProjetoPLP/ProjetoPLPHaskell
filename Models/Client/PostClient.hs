module Models.Client.PostClient where

import Models.Client.SaveClient ( getClient, getClientJSON )
import Models.Client.ModelClient ( createAsset, Asset(qtd, companyID), Client(ident, allAssets) )
import Models.Client.GetSetAttrsClient ( getAllAssets, setAllAssets )


-- Adiciona a um cliente X uma quantidade Z de uma empresa Y
addAsset :: Int -> Int -> Int -> IO ()
addAsset clientID companyID qtd = do
    let client = getClient clientID

    if ident client /= -1 then do
        let recoveryAssetsClient = allAssets client

        if qtd >= 0 then do
            if existAssetInClient (allAssets client) companyID then do
                let newExistentAssets = addExistentAssetInCompany recoveryAssetsClient companyID qtd
                setAllAssets clientID newExistentAssets

            else do
                let newAllAssets = createAsset companyID qtd : recoveryAssetsClient
                if length newAllAssets <= 11 then do
                    setAllAssets clientID newAllAssets
                else do
                    putStrLn "\nOcorreu um  probelama! Quantidade de ações excedida."

        else do
            if existAssetInClient (allAssets client) companyID then do
                let newExistentAssets = addExistentAssetInCompany recoveryAssetsClient companyID qtd
                let removed = removeAssetsNegative newExistentAssets
                setAllAssets clientID removed

            else do
                putStrLn "\nOcorreu um problema! O Cliente com este id não foi encontrado!"

    else do
        putStrLn "\nOcorreu um problema! O Cliente com este id não foi encontrado!"


existAssetInClient :: [Asset] -> Int -> Bool
existAssetInClient [] _ = False
existAssetInClient (x:xs) id
    | companyID x == id = True
    | otherwise = existAssetInClient xs id


addExistentAssetInCompany :: [Asset] -> Int -> Int -> [Asset]
addExistentAssetInCompany [] _ _= []
addExistentAssetInCompany (x:xs) idCompany qtd = addQtd x idCompany qtd : addExistentAssetInCompany xs idCompany qtd


addQtd :: Asset -> Int -> Int -> Asset
addQtd asset idCompany qtd_
    | companyID asset == idCompany = asset { qtd = qtd asset + qtd_ }
    | otherwise = asset


removeAssetsNegative :: [Asset] -> [Asset]
removeAssetsNegative [] = []
removeAssetsNegative (x:xs) = (remove x) ++ removeAssetsNegative xs


remove :: Asset -> [Asset]
remove asset = [asset | qtd asset > 0]


-- Remove da carteira de todos os clientes uma empresa
removeAllClientsAsset :: Int -> IO ()
removeAllClientsAsset idComp = removeAllClientsAssetAux idComp (getClientJSON "./Data/Clients.json")


removeAllClientsAssetAux :: Int -> [Client] -> IO ()
removeAllClientsAssetAux idComp [] = return ()
removeAllClientsAssetAux idComp (x:xs) = do
    let idUser = ident x
    removeClientAsset idUser idComp (getAllAssets idUser)
    removeAllClientsAssetAux idComp xs


removeClientAsset :: Int -> Int -> [Asset] -> IO ()
removeClientAsset idUser idComp [] = return ()
removeClientAsset idUser idComp (x:xs) = do
  if companyID x == idComp then
    addAsset idUser idComp (-(qtd x))
  else
    removeClientAsset idUser idComp xs