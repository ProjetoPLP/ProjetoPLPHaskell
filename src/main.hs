import Utils.MatrixUtils 
import Client.RealizarLogin
import System.IO (hFlush, stdout)
import Client.CreateClient
import Client.CadastrarCliente
import Company.CadastrarCompany

fazerLoginGeral :: IO ()
fazerLoginGeral = do
   printMatrix "./Wallet/login_menu.txt"
   resultadoLogin <- fazerLogin
   putStrLn resultadoLogin

cadastraUsuario:: IO()
cadastraUsuario = do
   printMatrix "./Wallet/sign-in_menu_usuario.txt"
   cadastraCliente <- cadastrarCliente
   putStrLn cadastraCliente

cadastraEmpresa:: IO()
cadastraEmpresa = do
   printMatrix "./Wallet/sign-in_menu_empresa.txt"
   cadastraEmpresa <- cadastrarCompany
   print cadastraEmpresa

menuStart :: IO()
menuStart = do
   printMatrix "./Wallet/start_menu.txt"
   putStr "Digite uma opção: "
   hFlush stdout
   input <- getLine
   if input == "L" then
      fazerLoginGeral
   else if input == "U" then
      cadastraUsuario
   else if input == "E" then
      cadastraEmpresa
   else if input == "S" then
      return()
   else do
      putStrLn "Opção Inválida!"
      menuStart

main:: IO()
main = do
    menuStart
