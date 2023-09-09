import Utils.MatrixUtils
import Wallet.WalletUpdate
import Company.GetSetAttrsCompany (getTrendIndicator)
import Client.GetSetAttrsClient

main :: IO ()
main = do

    -- updateWLStockName "./Client/Wallet/wallet2.txt" 10 "VALE3"

    -- updateWLStockPrice "./Client/Wallet/wallet2.txt" 7 10 (getTrendIndicator 1)

    -- updateWLOwnedStock "./Client/Wallet/wallet2.txt" 10 99999

    updateClientWallet 1

    printMatrix "./Client/Wallet/wallet1.txt"

    -- print (getCash 1)

