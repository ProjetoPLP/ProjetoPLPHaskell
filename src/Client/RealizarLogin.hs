module Client.RealizarLogin where
import Client.GetInfoForMakeLogin
import Client.SaveClient
import Client.ModelClient

fazerLogin :: IO Bool
fazerLogin = do
    email <- getEmail
    passwordClient <- getPassword

    let client = searchAndGetClientByEmail email
    hasData <- hasLoginDataInJSON
    
    if not hasData then do
        if length (show passwordClient) == 5 then do
            if not (ident client == (-1)) then do
                if (password client) == passwordClient then do
                    saveLoginJSON client
                    putStrLn "\nLogin realizado!"
                    return True
                else do 
                    putStrLn "\nSenha incorreta!"
                    return False
            else do
                putStrLn "\nOcorreu um  probelama! O email não existe."
                return False
        else do
            putStrLn "\nOcorreu um problema! A senha deve ter 5 digitos."
            return False
    else do
        putStrLn "\nOcorreu um problema! Você já está logado, saia da sessão para logar novamente."
        return False

searchAndGetClientByEmail :: String -> Client
searchAndGetClientByEmail email = verifingIfExistEmailClient email (getClientJSON "./Data/Clients.json")

verifingIfExistEmailClient :: String -> [Client] -> Client
verifingIfExistEmailClient _ [] = Client (-1) "" 0 0 "" 0 0 0.00 False 19 52
verifingIfExistEmailClient emailClient (head:tail) = 
  if emailClient == (email head) then head
  else verifingIfExistEmailClient emailClient tail
