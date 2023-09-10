import Control.Concurrent (threadDelay)
import System.Random (randomRIO)
import Data.Time.Clock (getCurrentTime, UTCTime, addUTCTime)

import HomeBroker.HomeBrokerUpdate
import Utils.MatrixUtils (printMatrix)

import Company.GetSetAttrsCompany
import Clock.ClockUpdate
import HomeBroker.HomeBrokerLoopLogic (callLoop)
import Wallet.WalletAttPatrimony (getNewPatrimony)


main :: IO ()
main = do
    callLoop 2 5
    -- patri <- getNewPatrimony 1
    -- print ()