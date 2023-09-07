module Client.RealizarLogin where
import Client.GetInfoForMakeLogin
import Client.SaveClient
import Client.ModelClient

fazerLogin :: IO String
fazerLogin = do
    email <- getEmail
    passwordClient <- getPassword

    let client = searchAndGetClientByEmail email
    hasData <- hasLoginDataInJSON
    
    if not hasData then do
        if length (show passwordClient) == 5 then do
            if not (identifier client == (-1)) then do
                if (password client) == passwordClient then do
                    saveLoginJSON client
                    return "\nLogin realizado!"
                else return "\nSenha incorreta!"
            else return "\nOcorreu um problema! O email não existe."
        else return "\nOcorreu um problema! A senha deve ter 5 dígitos."
    else return "\nOcorreu um problema! Você já está logado, saia da sessão para logar novamente."

searchAndGetClientByEmail :: String -> Client
searchAndGetClientByEmail email = verifingIfExistEmailClient email (getClientJSON "./Data/Clients.json")

verifingIfExistEmailClient :: String -> [Client] -> Client
verifingIfExistEmailClient _ [] = Client (-1) "" 0 0 "" 0 0.00
verifingIfExistEmailClient emailClient (head:tail) = 
  if emailClient == (email head) then head
  else verifingIfExistEmailClient emailClient tail
