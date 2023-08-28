module GetInfoForCreateCompany where

getName :: IO String
getName = do
  putStr "Digite o seu nome: "
  input <- getLine
  return input

getAgeFounded :: IO Int
getAgeFounded = do
  putStr "Digite o ano de fundação: "
  input <- getLine
  let age = (read input :: Int)
  return age

getCNPJ :: IO Int
getCNPJ = do
  putStr "Digite o seu CNPJ da empresa: "
  input <- getLine
  let cpf = (read input :: Int)
  return cpf

getActuation :: IO String
getActuation = do
  putStr "Digite a área de atuação da empresa: "
  input <- getLine
  return input

getDeclaration :: IO String
getDeclaration = do
  putStr "Digite a declaração de missão: "
  input <- getLine
  return input

getPassword :: IO Int
getPassword = do
  putStr "Digite o código de verificação de 5 dígitos: "
  input <- getLine
  let password = (read input :: Int)
  return password
