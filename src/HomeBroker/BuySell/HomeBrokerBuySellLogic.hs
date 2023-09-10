module HomeBroker.BuySell.HomeBrokerBuySellLogic where
import Client.GetSetAttrsClient (getQtdAssetsInCompany, getCash, removeCash, addCash)
import Company.GetSetAttrsCompany (getPrice)
import Client.PostClient (addAsset)


buy :: Int -> Int -> Int -> IO ()
buy idClient idComp num = do
    let cash = getCash idClient
        totalPrice = getPrice idComp * fromIntegral num

    if totalPrice > cash then return ()
    else do
        removeCash idClient totalPrice
        addAsset idClient idComp num


sell :: Int -> Int -> Int -> IO ()
sell idClient idComp num = do
    let cash = getCash idClient
        totalPrice = getPrice idComp * fromIntegral num

    if num > getQtdAssetsInCompany idClient idComp then putStrLn "Ações insuficientes"
    else do
        addCash idClient totalPrice
        addAsset idClient idComp (-num)
