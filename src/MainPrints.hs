import Utils.MatrixUtils

import MainMenu.MainMenuUpdate

main :: IO ()
main = do

    updateMainMenu 1

    printMatrix "./MainMenu/mainMenu.txt"

