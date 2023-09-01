module GetInfoForCreateCompany where
import System.IO (hFlush, stdout) 

getName :: IO String
getName = do
  putStr "Digite o nome da empresa: "
  hFlush stdout
  input <- getLine
  return input

getAgeFounded :: IO Int
getAgeFounded = do
  putStr "Digite o ano de fundação da empresa: "
  hFlush stdout
  input <- getLine
  let age = (read input :: Int)
  return age

getCNPJ :: IO Int
getCNPJ = do
  putStr "Digite o seu CNPJ da empresa: "
  hFlush stdout
  input <- getLine
  let cpf = (read input :: Int)
  return cpf

getActuation :: IO String
getActuation = do
  putStr "Digite a área de atuação da empresa: "
  hFlush stdout
  input <- getLine
  return input

getDeclaration :: IO String
getDeclaration = do
  putStr "Digite a declaração de missão da empresa: "
  hFlush stdout
  input <- getLine
  return input

getCode :: IO Int
getCode = do
  putStr "Digite o código de verificação de 5 dígitos da empresa: "
  hFlush stdout
  input <- getLine
  let code = (read input :: Int)
  return code
