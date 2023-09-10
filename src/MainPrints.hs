import Utils.MatrixUtils

import MainMenu.MainMenuUpdate
import Wallet.WalletUpdate (updateClientWallet, updateWalletDeposito, updateWalletSaque)
import MainMenu.CompanyDescription.CompanyDescriptionUpdate (updateCompanyDescription)
import HomeBroker.HomeBrokerUpdate
import Utils.VerificationUtils (existCompany)

main :: IO ()
main = do

    -- updateMainMenu 1
    -- printMatrix "./MainMenu/mainMenu.txt"

    updateClientWallet 1
    printMatrix "./Client/Wallet/wallet1.txt"

    -- print (existCompany 3)

    -- updateCompanyDescription 1 3
    -- printMatrix "./MainMenu/CompanyDescription/companyDescription.txt"

    -- updateHomeBroker 1 2
    -- printMatrix "./Company/HomeBroker/homebroker2.txt"

    -- updateWalletDeposito 1
    -- printMatrix "./Wallet/DepositoSaque/walletDeposito.txt"

    -- updateWalletSaque 1
    -- printMatrix "./Wallet/DepositoSaque/walletSaque.txt"
