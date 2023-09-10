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

walletUser :: Int -> IO()
walletUser id = do
   updateClientWallet id
   printMatrix ("./Client/Wallet/wallet" ++ (show id) ++ ".txt")

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
      
descricaoDaEmpresa :: Int -> Int -> IO()
descricaoDaEmpresa idCompany idUser = do
   updateCompanyDescription idUser idCompany
   printMatrix "./MainMenu/CompanyDescription/companyDescription.txt"

menuPrincipal:: IO()
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
      logoutClient
      return()
   else do
      putStrLn "Opção Inválida!"
      menuStart

main:: IO()
main = do
    menuStart