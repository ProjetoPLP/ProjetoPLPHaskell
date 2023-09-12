module Client.PostClient where
import Client.SaveClient
import Client.ModelClient
import Client.GetSetAttrsClient

-- ====================== addAsset =========================== --
-- Entrada: id: int / companyID: Int / price: Float
-- TipoDeSaida: Bool
addAsset :: Int -> Int -> Int -> IO ()
addAsset clientID companyID qtd = do
    let client = getClient clientID

    if (ident client) /= -1 then do
        let recoveryAssetsClient = allAssets client

        if qtd >= 0 then do
            if (existAssetInClient (allAssets client) companyID) then do
                let newExistentAssets = addExistentAssetInCompany recoveryAssetsClient companyID qtd
                setAllAssets clientID newExistentAssets

            else do
                let newAllAssets = [(createAsset companyID qtd)] ++ recoveryAssetsClient
                if (length newAllAssets) <= 11 then do
                    setAllAssets clientID newAllAssets
                else do
                    putStrLn "\nOcorreu um  probelama! Quantidade de ações excedida."

        else do
            if (existAssetInClient (allAssets client) companyID) then do
                let newExistentAssets = addExistentAssetInCompany recoveryAssetsClient companyID qtd
                let removed = removeAssetsNegative newExistentAssets
                setAllAssets clientID removed

            else do
                putStrLn "\nOcorreu um problema! O Cliente com este id não foi encontrado!"

    else do
        putStrLn "\nOcorreu um problema! O Cliente com este id não foi encontrado!"


existAssetInClient :: [Asset] -> Int -> Bool
existAssetInClient [] _ = False
existAssetInClient (x:xs) id =
    if (companyID x) == id then
        True
    else
        existAssetInClient xs id

addExistentAssetInCompany :: [Asset] -> Int -> Int -> [Asset]
addExistentAssetInCompany [] _ _= []
addExistentAssetInCompany (x:xs) idCompany qtd = [addQtd x idCompany qtd] ++ addExistentAssetInCompany xs idCompany qtd

addQtd :: Asset -> Int -> Int -> Asset
addQtd asset idCompany qtd_ =
    if companyID asset == idCompany then
        asset { qtd = qtd asset + qtd_ }
    else
        asset

removeAssetsNegative :: [Asset] -> [Asset]
removeAssetsNegative [] = []
removeAssetsNegative (x:xs) = (remove x) ++ removeAssetsNegative xs

remove :: Asset -> [Asset]
remove asset =
    if (qtd asset) <= 0 then []
    else [asset]
