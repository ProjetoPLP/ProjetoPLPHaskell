import Utils.MatrixUtils 
import Data.Char (isUpper, isLower)
import Client.RealizarLogin
import System.IO (hFlush, stdout)
import Client.CreateClient
import Client.CadastrarCliente
import Company.CadastrarCompany
import Control.Concurrent (threadDelay)

walletUser:: IO()
walletUser = do
   printMatrix "./Sprites/Wallet/wallet_base.txt"

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

menuPrincipal:: IO()
menuPrincipal = do
   printMatrix "./Sprites/MainMenu/mainMenu_base.txt"
   putStr "Digite uma opção: "
   hFlush stdout
   respostaUser <- getLine
   if respostaUser == "W" ||respostaUser == "w"then
      walletUser
      -- updateClientWallet
   else if respostaUser == "S" ||respostaUser == "s"then
      menuStart
   else do
      putStrLn "Opção Inválida!"
      menuPrincipal

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
   else if input == "S" || input == "s"then
      return()
   else do
      putStrLn "Opção Inválida!"
      menuStart

main:: IO()
main = do
    menuStart