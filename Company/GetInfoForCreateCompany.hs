module Company.GetInfoForCreateCompany where
import System.IO (hFlush, stdout) 
import Data.Char (toUpper)
import Utils.VerificationUtils (isNumber)
import Company.SaveCompany (existCompanyByName)


getName :: IO String
getName = do
    putStr "Digite o nome da empresa: "
    hFlush stdout
    name <- getLine
    if length name > 18 then do
        putStrLn "\nAviso: O nome da empresa deve ter no máximo 18 caracteres."
        getName
    else if existCompanyByName name then do
        putStrLn "\nAviso: A empresa já foi cadastrada."
        getName
    else return name


getAgeFounded :: IO String
getAgeFounded = do
    putStr "Digite o ano de fundação da empresa: "
    hFlush stdout
    age <- getLine
    if not (isNumber age) then do
        putStrLn "\nAviso: o ano de fundação deve possuir apenas números."
        getAgeFounded
    else return age


getCNPJ :: IO String
getCNPJ = do
    putStr "Digite o CNPJ da empresa (apenas números, 14 dígitos): "
    hFlush stdout
    cnpj <- getLine
    if not (isNumber cnpj) then do
        putStrLn "\nAviso: o CNPJ deve possuir apenas números."
        getCNPJ
    else if length cnpj /= 14 then do
        putStrLn "\nAviso: O CPNJ não contém 14 dígitos."
        getCNPJ
    else return cnpj


getActuation :: IO String
getActuation = do
    putStr "Digite a área de atuação da empresa: "
    hFlush stdout
    actuation <- getLine
    if length actuation > 29 then do
        putStrLn "\nAviso: A área de atuação da empresa deve ter no máximo 29 caracteres."
        getActuation
    else return actuation


getDeclaration :: IO String
getDeclaration = do
    putStr "Digite a declaração de missão da empresa: "
    hFlush stdout
    declaration <- getLine
    if length declaration > 86 then do
        putStrLn "\nAviso: A declaração de missão da empresa deve ter no máximo 86 caracteres."
        getDeclaration
    else return declaration


getCode :: IO String
getCode = do
    putStr "Digite o código da ação (deve seguir o modelo VALE3): "
    hFlush stdout
    code <- getLine
    if length code /= 5 then do
        putStrLn "\nAviso: O código da ação deve possuir 5 caracteres."
        getCode
    else return (uppercaseString code)


uppercaseString :: String -> String
uppercaseString = map toUpper