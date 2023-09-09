import Utils.MatrixUtils

import MainMenu.MainMenuUpdate
import Wallet.WalletUpdate (updateClientWallet)
import MainMenu.CompanyDescription.CompanyDescriptionUpdate (updateCompanyDescription)

main :: IO ()
main = do

    updateMainMenu 1
    printMatrix "./MainMenu/mainMenu.txt"

    -- updateClientWallet 1
    -- printMatrix "./Client/Wallet/wallet1.txt"

    -- updateCompanyDescription 1 5
    -- printMatrix "./MainMenu/CompanyDescription/companyDescription.txt"

