module Client.PostClient where
import Client.SaveClient
import Client.ModelClient
import Client.GetSetAttrsClient

-- ====================== addAsset =========================== --
-- Entrada: id: int / companyID: Int / price: Float
-- TipoDeSaida: Bool
addAsset :: Int -> Int -> Float -> IO Bool
addAsset clientID companyID price = do
    let client = getClient clientID

    if (ident client) /= -1 then do    
        let recoveryAssetsClient = allAssets client

        if (existPriceInCompany (allAssets client) companyID) then do
            let newExistentAssets = addExistentAssetInCompany recoveryAssetsClient companyID price
            setAllAssets clientID newExistentAssets
            putStrLn ("\nOlá" ++ (name client) ++ "! A sua ação na Empresa " ++ "(code company)" ++ "foi concluída e incrementada!")
            return True

        else do
            let newAllAssets = [(createAsset companyID price)] ++ recoveryAssetsClient
            if (length newAllAssets) <= 11 then do
                setAllAssets clientID newAllAssets
                putStrLn ("\nOlá " ++ (name client) ++ "! A sua ação na Empresa " ++ "(code company)" ++ "foi concluída!")
                return True
            else do
                putStrLn "\nOcorreu um  probelama! Quantidade de ações excedida."
                return False
    else do
        putStrLn "\nOcorreu um problema! O Cliente com este id não foi encontrado!"
        return False

existPriceInCompany :: [Asset] -> Int -> Bool
existPriceInCompany [] _ = False
existPriceInCompany (x:xs) id =
    if (companyID x) == id then
        True
    else
        existPriceInCompany xs id

addExistentAssetInCompany :: [Asset] -> Int -> Float -> [Asset]
addExistentAssetInCompany [] _ _= []
addExistentAssetInCompany (x:xs) idCompany value = [addPrice x idCompany value] ++ addExistentAssetInCompany xs idCompany value

addPrice :: Asset -> Int -> Float -> Asset
addPrice asset idCompany priceToAdd =
    if (companyID asset) == idCompany then
        asset { price = (price asset) + priceToAdd }
    else
        asset
