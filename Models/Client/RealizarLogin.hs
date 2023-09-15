module Models.Client.RealizarLogin where

import Models.Client.GetInfoForMakeLogin ( getEmail, getPassword )
import Models.Client.SaveClient ( getClientJSON )
import Models.Client.ModelClient ( Client(Client, email, ident, password) )
import Models.Client.LoginClient ( saveLogin )


fazerLogin :: IO Bool
fazerLogin = do
    email <- getEmail
    passwordClient <- getPassword

    let client = searchAndGetClientByEmail email
    if ident client /= (-1) then do
        if password client == passwordClient then do
            saveLogin client
            return True
        else do
            putStrLn "Aviso: Senha incorreta."
            return False
    else do
        putStrLn "Aviso: E-mail não cadastrado."
        return False


searchAndGetClientByEmail :: String -> Client
searchAndGetClientByEmail email = verifingIfExistEmailClient email (getClientJSON "./Data/Clients.json")


verifingIfExistEmailClient :: String -> [Client] -> Client
verifingIfExistEmailClient _ [] = Client (-1) "" "" "" "" "" 0 0 " " False 19 52 []
verifingIfExistEmailClient emailClient (head:tail)
    | emailClient == email head = head
    | otherwise = verifingIfExistEmailClient emailClient tail