import HomeBroker.HomeBrokerUpdate
import HomeBroker.HomeBrokerGraphUpdate
import Wallet.WalletUpdate
import Wallet.WalletGraphUpdate
import Utils.MatrixUtils
import Clock.Clock

import Client.GetAttrsClient

main :: IO ()
main = do

    -- HomeBroker
    -- updateHBGraphCandle "./HomeBroker/homebroker.txt" 20 4
    -- updateHBStockPrice "./HomeBroker/homebroker.txt" 30.5
    -- updateHBStockMaxPrice "./HomeBroker/homebroker.txt" 1.5
    -- updateHBStockMinPrice "./HomeBroker/homebroker.txt" 21
    -- updateHBStockStartPrice "./HomeBroker/homebroker.txt" 23.5
    -- updateHBCash "./HomeBroker/homebroker.txt" 1.5
    -- updateHBOwnedStocks "./HomeBroker/homebroker.txt" 2
    -- updateHBStockName "./HomeBroker/homebroker.txt" "VALE3"
    -- updateHBCompanyName "./HomeBroker/homebroker.txt" "Vale S.A"

    -- Wallet
    -- updateWLGraphCandle "./Wallet/wallet.txt" 19 52
    updateWLCash "./Wallet/wallet.txt" (getSaldo 1)
    -- updateWLAssets "./Wallet/wallet.txt" 500.0
    -- updateWLStockName "./Wallet/wallet.txt" 2 "PETR4"
    -- updateWLStockPrice "./Wallet/wallet.txt" 2 27.0
    -- updateWLOwnedStock "./Wallet/wallet.txt" 1 1
    updateWLUserName "./Wallet/wallet.txt" (getNome 1)
    updateWLUserCPF "./Wallet/wallet.txt" (show (getCPF 1))
    -- updateWLNewsPercent "./Wallet/wallet.txt" 10
    -- updateWLNewsText "./Wallet/wallet.txt" "valorizaram"

    -- Clock
    -- updateMatrixClock "./HomeBroker/homebroker.txt" 20
    
    -- printMatrix "./HomeBroker/homebroker.txt"
    printMatrix "./Wallet/wallet.txt"



    -- updateWLCash "./Wallet/wallet.txt" 100