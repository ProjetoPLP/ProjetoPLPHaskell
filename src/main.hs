import Utils.MatrixUtils 
import Data.Char (isUpper, isLower)
import Client.RealizarLogin
import System.IO (hFlush, stdout)
import Client.CreateClient
import Client.CadastrarCliente
import Company.CadastrarCompany
import Control.Concurrent (threadDelay)
import Client.GetSetAttrsClient
import MainMenu.MainMenuUpdate
import Client.SaveClient
import MainMenu.CompanyDescription.CompanyDescriptionUpdate
import Wallet.WalletUpdate
import Utils.VerificationUtils (existCompany)
import Wallet.DepositoSaque.WalletDepSaqLogic
import HomeBroker.HomeBrokerUpdate
import HomeBroker.BuySell.HomeBrokerBuySellLogic
import Data.Char (isDigit)
import HomeBroker.HomeBrokerLoopLogic

isNumber :: String -> Bool
isNumber = all isDigit

stringToInt :: String -> Int
stringToInt str = read str

operacaoSacar :: Int -> IO()
operacaoSacar id = do
   updateWalletSaque id
   printMatrix "./Wallet/DepositoSaque/walletSaque.txt"
   putStr "Digite uma opção: "
   hFlush stdout
   respostaUser <- getLine
   opUserSacar id respostaUser


opUserSacar :: Int -> String -> IO()
opUserSacar id respostaUser 
   | respostaUser == "5" = do
      sacar500 id
      operacaoSacar id
   | respostaUser == "1" = do
      sacar1000 id
      operacaoSacar id
   | respostaUser == "T" || respostaUser == "t" = do
      sacarTudo id
      operacaoSacar id
   | respostaUser == "V" || respostaUser == "v" = walletUser id
   | otherwise = do
      putStrLn "Opção inválida"
      operacaoSacar id
      

opUserDeposito :: Int -> String -> IO()
opUserDeposito id respostaUser 
   | respostaUser == "S" || respostaUser  == "s" = do
       depositar id (getCanDeposit id)
       operacaoDepositar id
   | respostaUser == "V" || respostaUser  == "v" = walletUser id
   | otherwise = do
         putStrLn "Opção inválida"
         operacaoDepositar id

operacaoDepositar :: Int -> IO()
operacaoDepositar id = do
   updateWalletDeposito id
   printMatrix "./Wallet/DepositoSaque/walletDeposito.txt"
   putStr "Digite uma opção (S/V): "
   hFlush stdout
   respostaUser <- getLine
   opUserDeposito id respostaUser

opcoesWallet :: Int -> String -> IO()
opcoesWallet id opcao 
   | opcao == "S" || opcao == "s" = operacaoSacar id 
   | opcao == "D" || opcao == "d" = operacaoDepositar id 
   | opcao == "V" || opcao == "v" = menuPrincipal
   | otherwise = do
           putStrLn "Opção inválida"
           walletUser id

walletUser :: Int -> IO()
walletUser id = do
   updateClientWallet id
   printMatrix ("./Client/Wallet/wallet" ++ (show id) ++ ".txt")
   putStr "D - Depósito\nS - Saque\nV - Voltar\nDigite uma opção: "
   hFlush stdout
   respostaUser <- getLine
   opcoesWallet id respostaUser
 
ehcadastrado:: Bool -> IO()
ehcadastrado result = do
   if result then do
      cadastroRealizado
      threadDelay (1 * 2000000)
      menuStart
   else
      menuStart

cadastroRealizado :: IO()
cadastroRealizado = do
   printMatrix "./Sprites/StartMenu/sign-in_menu_cadastro_realizado.txt"

opcoesMenu :: String -> Int -> IO()
opcoesMenu op id
   | op == "W" || op == "w" = walletUser id
   | op == "1" = descricaoDaEmpresa 1 id
   | op == "2" = descricaoDaEmpresa 2 id
   | op == "3" = descricaoDaEmpresa 3 id
   | op == "4" = descricaoDaEmpresa 4 id
   | op == "5" = descricaoDaEmpresa 5 id
   | op == "6" = descricaoDaEmpresa 6 id
   | op == "7" = descricaoDaEmpresa 7 id
   | op == "8" = descricaoDaEmpresa 8 id
   | op == "9" = descricaoDaEmpresa 9 id
   | op == "A" = descricaoDaEmpresa 10 id
   | op == "B" = descricaoDaEmpresa 11 id
   | op == "C" = descricaoDaEmpresa 12 id
   | op == "S" = menuStart
   | otherwise = do
           putStrLn "Opção inválida"
           menuPrincipal

opBuyMenu :: String -> Int -> Int -> IO()
opBuyMenu respostaUser idUser idCompany
   | isNumber respostaUser = do
      buy idUser idCompany (stringToInt respostaUser)
      buyMenu idUser idCompany
   | respostaUser == "V" || respostaUser == "v" = homeBrokerUser idCompany idUser
   | otherwise = do
      putStrLn "Opção Inválida!"
      buyMenu idUser idCompany

buyMenu :: Int -> Int -> IO()
buyMenu idUSer idCompany = do
   updateHomeBrokerBuy idUSer idCompany
   printMatrix "./HomeBroker/BuySell/homebrokerBuy.txt"
   putStr "Digite quantas ações deseja comprar: "
   hFlush stdout
   respostaUser <- getLine
   opBuyMenu respostaUser idUSer idCompany

opSellMenu :: String -> Int -> Int -> IO()
opSellMenu respostaUser idUser idCompany 
   | isNumber respostaUser = do
      sell idUser idCompany (stringToInt respostaUser)
      sellMenu idUser idCompany
   | respostaUser == "V" || respostaUser == "v" = homeBrokerUser idCompany idUser
   | otherwise = do
      putStrLn "Opção Inválida!"
      sellMenu idUser idCompany

sellMenu :: Int -> Int -> IO()
sellMenu idUSer idCompany = do
   updateHomeBrokerSell idUSer idCompany
   printMatrix "./HomeBroker/BuySell/homebrokerSell.txt"
   putStr "Digite quantas ações deseja vender: "
   hFlush stdout
   respostaUser <- getLine
   opSellMenu respostaUser idUSer idCompany

opHomeBrokerCompany :: String -> Int -> Int -> IO()
opHomeBrokerCompany op idUser idCompany
   | op == "B" || op == "b" = buyMenu idUser idCompany
   | op == "S" || op == "s" = sellMenu idUser idCompany
   | op == "V" || op == "v" = descricaoDaEmpresa idCompany idUser
   | isNumber op = do
      callLoop idCompany (stringToInt op)
      homeBrokerUser idCompany idUser
   | otherwise  = do
           putStrLn "Opção inválida"
           homeBrokerUser idCompany idUser

homeBrokerUser :: Int -> Int -> IO()
homeBrokerUser idCompany idUser = do
   updateHomeBroker idUser idCompany
   printMatrix  ("./Company/HomeBroker/homebroker" ++ (show idCompany) ++ ".txt")
   putStr "Digite uma opção: "
   hFlush stdout
   respostaUser <- getLine
   opHomeBrokerCompany respostaUser idUser idCompany
   

descricaoDaEmpresa :: Int -> Int -> IO()
descricaoDaEmpresa idCompany idUser = do
   if existCompany idCompany then do
      updateCompanyDescription idUser idCompany
      printMatrix "./MainMenu/CompanyDescription/companyDescription.txt"
      putStr "Digite uma opção: "
      hFlush stdout
      respostaUser <- getLine
      if respostaUser == "H" || respostaUser  == "h" then
         homeBrokerUser idCompany idUser
      else if respostaUser == "V" || respostaUser == "v" then do
         menuPrincipal   
      else do
         putStrLn "Opção Inválida!"
         descricaoDaEmpresa idCompany idUser
   else
      menuPrincipal

menuPrincipal :: IO()
menuPrincipal = do
   myId <- getID
   updateMainMenu myId
   printMatrix "./MainMenu/mainMenu.txt"
   putStr "Digite uma opção: "
   hFlush stdout
   respostaUser <- getLine
   opcoesMenu respostaUser myId

fazerLoginGeral :: IO ()
fazerLoginGeral = do
   printMatrix "./Sprites/StartMenu/login_menu.txt"
   resposta <- querContinuarAOperacao
   if resposta then do
      resultadoLogin <- fazerLogin
      if resultadoLogin then
         menuPrincipal
      else
         menuStart
   else do
      menuStart

cadastraUsuario:: IO()
cadastraUsuario = do
   printMatrix "./Sprites/StartMenu/sign-in_menu_usuario.txt"
   resposta <- querContinuarAOperacao
   if resposta then do
      cadastraCliente <- cadastrarCliente
      ehcadastrado cadastraCliente
   else do
      menuStart


cadastraEmpresa:: IO()
cadastraEmpresa = do
   printMatrix "./Sprites/StartMenu/sign-in_menu_empresa.txt"
   resposta <- querContinuarAOperacao
   if resposta then do
      cadastraEmpresa <- cadastrarCompany
      ehcadastrado cadastraEmpresa
   else do
      menuStart

querContinuarAOperacao:: IO Bool
querContinuarAOperacao = do
   putStr "Quer continuar a operação ? (Responda com (V) para voltar ou (C) para continuar): "
   hFlush stdout
   op <- getLine
   if op == "C" || op == "c" then 
      return True
   else
      return False

menuStart :: IO()
menuStart = do
   logoutClient
   printMatrix "./Sprites/StartMenu/start_menu.txt"
   putStr "Digite uma opção: "
   hFlush stdout
   input <- getLine
   if input == "L" || input == "l" then do
      fazerLoginGeral
   else if input == "U" || input == "u"then
      cadastraUsuario
   else if input == "E" || input == "e"then
      cadastraEmpresa
   else if input == "S" || input == "s"then do
      return()
   else do
      putStrLn "Opção Inválida!"
      menuStart

main:: IO()
main = do
    menuStart