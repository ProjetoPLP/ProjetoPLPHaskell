module Client.RealizarLogin where
import Client.GetInfoForMakeLogin
import Client.SaveClient
import Client.ModelClient
import Client.LoginClient (saveLogin)

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
verifingIfExistEmailClient _ [] = Client (-1) "" "" "" "" "" 0 0.00 False 19 52 []
verifingIfExistEmailClient emailClient (head:tail) = 
  if emailClient == (email head) then head
  else verifingIfExistEmailClient emailClient tail
