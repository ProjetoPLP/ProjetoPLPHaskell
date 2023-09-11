import Control.Concurrent (threadDelay)
import System.Random (randomRIO)
import Data.Time.Clock (getCurrentTime, UTCTime, addUTCTime)

import HomeBroker.HomeBrokerUpdate
import Utils.MatrixUtils (printMatrix)

import Company.GetSetAttrsCompany
import Clock.ClockUpdate
import HomeBroker.HomeBrokerLoopLogic (callLoop)

import Client.GetSetAttrsClient (getAllAssets, getCanDeposit)
import HomeBroker.BuySell.HomeBrokerBuySellLogic (buy, sell)
import Wallet.WalletUpdate (updateClientWallet)
import Client.PostClient
import Client.ModelClient (Client(canDeposit))
import HomeBroker.TrendingClose.TrendingCloseUpdate (updateTrendingClose)
-- import HomeBroker.TrendingClose.TrendingCloseUpdate (getTrendVar)


main :: IO ()
main = do
    -- callLoop 1 5
    -- patri <- getNewPatrimony 1
    -- print ()

    -- sell 1 8 4

    -- addAsset 1 12 1
    -- updateClientWallet 1
    -- printMatrix "./Client/Wallet/wallet1.txt"

    -- sacar 1 "5"
    -- depositar 1 (getCanDeposit 1)
    -- updateClientWallet 1
    -- printMatrix "./Client/Wallet/wallet1.txt"

    -- sell 1 1 2 
    -- updateHomeBroker 1 1
    -- printMatrix "./Company/HomeBroker/homebroker1.txt"

    updateTrendingClose 1
    printMatrix "./HomeBroker/TrendingClose/trendingClose.txt"


