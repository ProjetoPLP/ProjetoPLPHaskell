module GetInfoForCreatePeople where

getName :: IO String
getName = do
  putStr "Digite o seu nome: "
  input <- getLine
  return input

getAge :: IO Int
getAge = do
  putStr "Digite a sua idade: "
  input <- getLine
  let age = (read input :: Int)
  return age

getCPF :: IO Int
getCPF = do
  putStr "Digite o seu CPF: "
  input <- getLine
  let cpf = (read input :: Int)
  return cpf

getEmail :: IO String
getEmail = do
  putStr "Digite o seu e-mail: "
  input <- getLine
  return input

getPassword :: IO Int
getPassword = do
  putStr "Digite a sua senha: "
  input <- getLine
  let password = (read input :: Int)
  return password
