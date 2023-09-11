module Client.GetInfoForCreateClient where
import System.IO (hFlush, stdout)

getName :: IO String
getName = do
  putStr "Digite o seu nome: "
  hFlush stdout
  input <- getLine
  return input

getAge :: IO Int
getAge = do
  putStr "Digite a sua idade: "
  hFlush stdout
  input <- getLine
  let age = (read input :: Int)
  return age

getCPF :: IO String
getCPF = do
  putStr "Digite o seu CPF (apenas números, 11 dígitos): "
  hFlush stdout
  input <- getLine
  return input

getEmail :: IO String
getEmail = do
  putStr "Digite o seu e-mail: "
  hFlush stdout
  input <- getLine
  return input

getPassword :: IO String
getPassword = do
  putStr "Digite a sua senha: "
  hFlush stdout
  input <- getLine
  let password = (read input :: String)
  return password
