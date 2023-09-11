module Company.GetInfoForCreateCompany where
import System.IO (hFlush, stdout) 
import Data.Char (toUpper)

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

getCNPJ :: IO String
getCNPJ = do
  putStr "Digite o seu CNPJ da empresa (apenas números, 14 dígitos): "
  hFlush stdout
  input <- getLine
  return input

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

getCode :: IO String
getCode = do
  putStr "Digite o código da empresa (apenas 5 dígitos): "
  hFlush stdout
  input <- getLine
  return (uppercaseString input)

uppercaseString :: String -> String
uppercaseString = map toUpper
