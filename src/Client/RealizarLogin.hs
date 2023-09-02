module Client.RealizarLogin where
import Client.GetInfoForMakeLogin
import Client.SaveClient
import Client.ModelClient

fazerLogin :: IO String
fazerLogin = do
    email <- getEmail
    passwordClient <- getPassword
    let client = searchAndGetClientByEmail email
    if length (show passwordClient) == 5 then do
        if not (identifier client == (-1)) then do
            if (password client) == passwordClient then do
                return "\nLogin realizado!"
            else return "\nSenha incorreta!"
        else return "\nOcorreu um  probelama! O email não existe."
    else return "\nOcorreu um problema. A senha deve ter 5 digitos."

searchAndGetClientByEmail :: String -> Client
searchAndGetClientByEmail email = verifingIfExistEmailClient email (getClientJSON "./Data/Clients.json")

verifingIfExistEmailClient :: String -> [Client] -> Client
verifingIfExistEmailClient _ [] = Client (-1) "" 0 0 "" 0 0.00
verifingIfExistEmailClient emailClient (head:tail) = 
  if emailClient == (email head) then head
  else verifingIfExistEmailClient emailClient tail
