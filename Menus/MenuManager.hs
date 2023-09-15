module Menus.MenuManager where

import System.IO ( hFlush, stdout )
import Control.Concurrent ( threadDelay )
import Utils.MatrixUtils ( printMatrix )
import Utils.VerificationUtils ( existCompany, isNumber )
import Models.Client.RealizarLogin ( fazerLogin )
import Models.Client.CadastrarCliente ( cadastrarCliente )
import Models.Client.GetSetAttrsClient ( getCanDeposit, getLoggedUserID )
import Models.Client.LoginClient ( logoutClient )
import Models.Company.CadastrarCompany ( cadastrarCompany )
import Models.Company.SaveCompany ( getCompanyJSON )
import Menus.Wallet.DepositoSaque.WalletDepSaqLogic ( depositar, sacar500, sacar200, sacarTudo )
import Menus.Wallet.WalletUpdate ( updateClientWallet, updateWalletDeposito, updateWalletSaque )
import Menus.HomeBroker.HomeBrokerUpdate ( updateHomeBroker, updateHomeBrokerBuy, updateHomeBrokerSell )
import Menus.HomeBroker.BuySell.HomeBrokerBuySellLogic ( buy, sell )
import Menus.HomeBroker.HomeBrokerLoopLogic ( callLoop )
import Menus.HomeBroker.CompanyProfile.CompanyProfileUpdate ( updateCompanyProfile )
import Menus.HomeBroker.TrendingClose.TrendingCloseUpdate ( updateTrendingClose )
import Menus.HomeBroker.CompanyDown.CompanyDownUpdate ( updateCompanyDown, isDown )
import Menus.MainMenu.MainMenuUpdate ( updateMainMenu )
import Models.Clock.GetSetClock ( getClock, setClock )


startMenu :: IO ()
startMenu = do
    logoutClient
    printMatrix "./Menus/StartMenu/startMenu.txt"
    putStr "Digite uma opção: "
    hFlush stdout
    userChoice <- getLine
    optionsStartMenu userChoice


optionsStartMenu :: String -> IO ()
optionsStartMenu userChoice
    | userChoice == "L" || userChoice == "l" = fazerLoginMenu
    | userChoice == "U" || userChoice == "u" = cadastraUsuarioMenu
    | userChoice == "E" || userChoice == "e" = cadastraEmpresaMenu
    | userChoice == "S" || userChoice == "s" = return ()
    | otherwise = do
        putStrLn "Opção Inválida!"
        startMenu


fazerLoginMenu :: IO ()
fazerLoginMenu = do
    printMatrix "./Menus/StartMenu/loginMenu.txt"
    putStr "Deseja fazer login? (S/N): "
    hFlush stdout
    userChoice <- getLine

    if querContinuarAOperacao userChoice then do
        resultadoLogin <- fazerLogin
        if resultadoLogin then do
            idUser <- getLoggedUserID
            mainMenu idUser
        else
            startMenu
    else
        startMenu


cadastraUsuarioMenu :: IO ()
cadastraUsuarioMenu = do
    printMatrix "./Menus/StartMenu/cadastroUsuario.txt"
    putStr "Deseja cadastrar um novo usuário? (S/N): "
    hFlush stdout
    userChoice <- getLine

    if querContinuarAOperacao userChoice then do
        cadastrarCliente
        menuCadastroRealizado True
    else
        startMenu


cadastraEmpresaMenu :: IO ()
cadastraEmpresaMenu = do
    printMatrix "./Menus/StartMenu/cadastroEmpresa.txt"
    putStr "Deseja cadastrar uma nova empresa? (S/N): "
    hFlush stdout
    userChoice <- getLine

    if querContinuarAOperacao userChoice  then do
         cadastrou <- cadastrarCompany (length (getCompanyJSON "./Data/Companies.json"))
         menuCadastroRealizado cadastrou
    else
        startMenu


querContinuarAOperacao :: String -> Bool
querContinuarAOperacao userChoice
    | userChoice == "S" || userChoice == "s" = True
    | otherwise = False


menuCadastroRealizado :: Bool -> IO ()
menuCadastroRealizado cadastrou
    | cadastrou = do
        printMatrix "./Menus/StartMenu/cadastroRealizado.txt"
        threadDelay 2000000
        startMenu
    | otherwise = do
        putStrLn "Aviso: limite máximo de empresas cadastradas atingido."
        startMenu


mainMenu :: Int -> IO ()
mainMenu idUser = do
    updateMainMenu idUser
    printMatrix "./Menus/MainMenu/mainMenu.txt"
    putStr "Digite uma opção: "
    hFlush stdout
    userChoice <- getLine
    optionsMainMenu idUser userChoice


optionsMainMenu :: Int -> String -> IO ()
optionsMainMenu idUser userChoice
    | userChoice == "W" || userChoice == "w" = walletMenu idUser
    | userChoice == "1" = homeBrokerMenu idUser 1
    | userChoice == "2" = homeBrokerMenu idUser 2
    | userChoice == "3" = homeBrokerMenu idUser 3
    | userChoice == "4" = homeBrokerMenu idUser 4
    | userChoice == "5" = homeBrokerMenu idUser 5
    | userChoice == "6" = homeBrokerMenu idUser 6
    | userChoice == "7" = homeBrokerMenu idUser 7
    | userChoice == "8" = homeBrokerMenu idUser 8
    | userChoice == "9" = homeBrokerMenu idUser 9
    | userChoice == "A" || userChoice == "a" = homeBrokerMenu idUser 10
    | userChoice == "B" || userChoice == "b" = homeBrokerMenu idUser 11
    | userChoice == "C" || userChoice == "c" = homeBrokerMenu idUser 12
    | userChoice == "S" || userChoice == "s" = startMenu
    | otherwise = do
        putStrLn "Opção inválida"
        mainMenu idUser


homeBrokerMenu :: Int -> Int -> IO ()
homeBrokerMenu idUser idComp
    | existCompany idComp = do
        updateHomeBroker idUser idComp
        printMatrix  ("./Models/Company/HomeBrokers/homebroker" ++ show idComp ++ ".txt")
        putStr "Digite por quantos segundos a ação deve variar: "
        hFlush stdout
        userChoice <- getLine
        optionsHomeBrokerMenu idUser idComp userChoice
    | otherwise = mainMenu idUser


optionsHomeBrokerMenu :: Int -> Int -> String -> IO ()
optionsHomeBrokerMenu idUser idComp userChoice
    | userChoice == "B" || userChoice == "b" = buyMenu idUser idComp
    | userChoice == "S" || userChoice == "s" = sellMenu idUser idComp
    | userChoice == "P" || userChoice == "p" = companyProfileMenu idUser idComp
    | userChoice == "V" || userChoice == "v" = mainMenu idUser
    | isNumber userChoice = do
        let seg = formatInputToSeconds userChoice
        callLoop idComp seg
        menuAfterLoop idUser idComp
    | otherwise = do
        putStrLn "Opção inválida"
        homeBrokerMenu idUser idComp


menuAfterLoop :: Int -> Int -> IO ()
menuAfterLoop idUser idComp
    | isDown idComp = companyDownMenu idUser idComp
    | getClock "./Data/Clock.json" >= 720 = trendingCloseMenu idUser
    | otherwise = homeBrokerMenu idUser idComp


companyProfileMenu :: Int -> Int -> IO ()
companyProfileMenu idUser idComp = do
    updateCompanyProfile idUser idComp
    printMatrix "./Menus/HomeBroker/CompanyProfile/companyProfile.txt"
    putStr "Digite uma opção: "
    hFlush stdout
    userChoice <- getLine
    optionsCompanyProfileMenu idUser idComp userChoice


optionsCompanyProfileMenu :: Int -> Int -> String -> IO ()
optionsCompanyProfileMenu idUser idComp userChoice
    | userChoice == "V" || userChoice == "v" = homeBrokerMenu idUser idComp
    | otherwise = do
        putStrLn "Opção Inválida!"
        companyProfileMenu idUser idComp


buyMenu :: Int -> Int -> IO ()
buyMenu idUser idComp = do
    updateHomeBrokerBuy idUser idComp
    printMatrix "./Menus/HomeBroker/BuySell/homebrokerBuy.txt"
    putStr "Digite quantas ações deseja comprar: "
    hFlush stdout
    userChoice <- getLine
    optionsBuyMenu idUser idComp userChoice


optionsBuyMenu :: Int -> Int -> String -> IO ()
optionsBuyMenu idUser idComp userChoice
    | isNumber userChoice = do
        buy idUser idComp (read userChoice)
        buyMenu idUser idComp
    | userChoice `elem` ["V", "v", "C", "c"] = homeBrokerMenu idUser idComp
    | otherwise = do
        putStrLn "Opção Inválida!"
        buyMenu idUser idComp


sellMenu :: Int -> Int -> IO ()
sellMenu idUser idComp = do
    updateHomeBrokerSell idUser idComp
    printMatrix "./Menus/HomeBroker/BuySell/homebrokerSell.txt"
    putStr "Digite quantas ações deseja vender: "
    hFlush stdout
    userChoice <- getLine
    optionsSellMenu idUser idComp userChoice


optionsSellMenu :: Int -> Int -> String -> IO ()
optionsSellMenu idUser idComp userChoice
    | isNumber userChoice = do
        sell idUser idComp (read userChoice)
        sellMenu idUser idComp
    | userChoice `elem` ["V", "v", "C", "c"] = homeBrokerMenu idUser idComp
    | otherwise = do
        putStrLn "Opção Inválida!"
        sellMenu idUser idComp


walletMenu :: Int -> IO ()
walletMenu idUser = do
    updateClientWallet idUser
    printMatrix ("./Models/Client/Wallets/wallet" ++ show idUser ++ ".txt")
    putStr "Digite uma opção: "
    hFlush stdout
    userChoice <- getLine
    optionsWalletMenu idUser userChoice


optionsWalletMenu :: Int -> String -> IO ()
optionsWalletMenu idUser userChoice
    | userChoice == "S" || userChoice == "s" = saqueMenu idUser
    | userChoice == "D" || userChoice == "d" = depositoMenu idUser
    | userChoice == "V" || userChoice == "v" = mainMenu idUser
    | otherwise = do
        putStrLn "Opção inválida"
        walletMenu idUser


saqueMenu :: Int -> IO ()
saqueMenu idUser = do
    updateWalletSaque idUser
    printMatrix "./Menus/Wallet/DepositoSaque/walletSaque.txt"
    putStr "Digite uma opção: "
    hFlush stdout
    userChoice <- getLine
    optionsSaqueMenu idUser userChoice


optionsSaqueMenu :: Int -> String -> IO ()
optionsSaqueMenu idUser userChoice
    | userChoice == "2" = do
        sacar200 idUser
        saqueMenu idUser
    | userChoice == "5" = do
        sacar500 idUser
        saqueMenu idUser
    | userChoice == "T" || userChoice == "t" = do
        sacarTudo idUser
        saqueMenu idUser
    | userChoice == "V" || userChoice == "v" = walletMenu idUser
    | otherwise = do
        putStrLn "Opção inválida"
        saqueMenu idUser


depositoMenu :: Int -> IO ()
depositoMenu idUser = do
    updateWalletDeposito idUser
    printMatrix "./Menus/Wallet/DepositoSaque/walletDeposito.txt"
    putStr "Digite uma opção: "
    hFlush stdout
    userChoice <- getLine
    optionsDepositoMenu idUser userChoice


optionsDepositoMenu :: Int -> String -> IO ()
optionsDepositoMenu idUser userChoice
    | userChoice == "S" || userChoice  == "s" = do
        depositar idUser (getCanDeposit idUser)
        depositoMenu idUser
    | userChoice `elem` ["V", "v", "N", "n"] = walletMenu idUser
    | otherwise = do
        putStrLn "Opção inválida"
        depositoMenu idUser


trendingCloseMenu :: Int -> IO ()
trendingCloseMenu idUser = do
    updateTrendingClose idUser
    setClock 420
    printMatrix "./Menus/HomeBroker/TrendingClose/trendingClose.txt"
    putStr "Digite uma opção: "
    hFlush stdout
    userChoice <- getLine
    mainMenu idUser


companyDownMenu :: Int -> Int -> IO ()
companyDownMenu idUser idComp = do
    updateCompanyDown idUser idComp
    printMatrix "./Menus/HomeBroker/CompanyDown/companyDown.txt"
    putStr "Digite uma opção: "
    hFlush stdout
    userChoice <- getLine
    mainMenu idUser


formatInputToSeconds :: String -> Int
formatInputToSeconds seg
    | read seg > 30 = 30
    | otherwise = read seg