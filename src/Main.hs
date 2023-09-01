import HomeBroker.HomeBrokerUpdate
import HomeBroker.HomeBrokerGraphUpdate
import Wallet.WalletUpdate
import Wallet.WalletGraphUpdate
import Utils.MatrixUtils
import Clock.Clock

main :: IO ()
main = do

    -- -- HomeBroker
    -- updateHBGraphCandle "./HomeBroker/homebroker.txt" 20 4
    -- updateHBStockPrice "./HomeBroker/homebroker.txt" 30.5
    -- updateHBStockMaxPrice "./HomeBroker/homebroker.txt" 1.5
    -- updateHBStockMinPrice "./HomeBroker/homebroker.txt" 21
    -- updateHBStockStartPrice "./HomeBroker/homebroker.txt" 23.5
    -- updateHBCash "./HomeBroker/homebroker.txt" 500
    -- updateHBOwnedStocks "./HomeBroker/homebroker.txt" 2
    -- updateHBStockName "./HomeBroker/homebroker.txt" "VALE3"
    -- updateHBCompanyName "./HomeBroker/homebroker.txt" "Vale S.A"

    -- -- Wallet
    -- updateWLGraphCandle "./Wallet/wallet.txt" 19 52
    -- updateWLCash "./Wallet/wallet.txt" 500
    -- updateWLAssets "./Wallet/wallet.txt" 500.0
    -- updateWLStockName "./Wallet/wallet.txt" 2 "PETR4"
    -- updateWLStockPrice "./Wallet/wallet.txt" 2 27.0
    -- updateWLOwnedStock "./Wallet/wallet.txt" 1 1
    -- updateWLUserName "./Wallet/wallet.txt" "Daniel Mantovani"
    -- updateWLUserCPF "./Wallet/wallet.txt" "123.456.789-10"
    -- updateWLNewsPercent "./Wallet/wallet.txt" 10
    -- updateWLNewsText "./Wallet/wallet.txt" "valorizaram"
    updateMatrixClock "./HomeBroker/homebroker.txt" 20
    
    printMatrix "./HomeBroker/homebroker.txt"
    -- printMatrix "./Wallet/wallet.txt"