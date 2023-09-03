import Control.Concurrent (threadDelay)
import System.Random (randomRIO)
import Data.Time.Clock (getCurrentTime, UTCTime, addUTCTime)

import HomeBroker.HomeBrokerUpdate
import Utils.MatrixUtils (printMatrix)

import Company.GetSetAttrsCompany
import Clock.ClockUpdate
import HomeBroker.HomeBrokerGraphUpdate


main :: IO ()
main = do
    attStockPriceFor 1 10
