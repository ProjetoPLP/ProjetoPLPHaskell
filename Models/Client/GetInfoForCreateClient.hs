module Models.Client.GetInfoForCreateClient where

import System.IO ( hFlush, stdout )
import Utils.VerificationUtils ( isNumber )
import Models.Client.SaveClient ( existClientByEmail )


getName :: IO String
getName = do
    putStr "Digite o seu nome: "
    hFlush stdout
    name <- getLine
    if length name > 18 then do
        putStrLn "\nAviso: O nome do usuário deve ter no máximo 18 caracteres."
        getName
    else return name


getAge :: IO String
getAge = do
    putStr "Digite a sua idade: "
    hFlush stdout
    age <- getLine
    if not (isNumber age) then do
        putStrLn "\nAviso: a idade deve possuir apenas números."
        getAge
    else if read age < 18 then do
        putStrLn "\nAviso: proibido menores de 18 anos"
        getAge
    else return age


getCPF :: IO String
getCPF = do
    putStr "Digite o seu CPF (apenas números, 11 dígitos): "
    hFlush stdout
    cpf <- getLine
    if not (isNumber cpf) then do
        putStrLn "\nAviso: o CPF deve possuir apenas números."
        getCPF
    else if length cpf /= 11 then do
        putStrLn "\nAviso: O CPF não contém 11 dígitos."
        getCPF
    else return cpf


getEmail :: IO String
getEmail = do
    putStr "Digite o seu e-mail: "
    hFlush stdout
    email <- getLine
    if existClientByEmail email then do
        putStrLn "\nAviso: O e-mail já foi cadastrado."
        getEmail
    else return email


getPassword :: IO String
getPassword = do
    putStr "Digite a sua senha: "
    hFlush stdout
    password <- getLine
    if length password < 5 then do
        putStrLn "\nAviso: A senha deve ter no mínimo 5 digitos."
        getPassword
    else return password