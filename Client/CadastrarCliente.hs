module Client.CadastrarCliente where

import Client.SaveClient ( saveClientJSON )
import Client.ModelClient ( createClient, Client )
import Client.GetInfoForCreateClient (getName, getAge, getCPF, getEmail, getPassword)


cadastrarCliente :: IO ()
cadastrarCliente = do
    client <- getNewClient
    saveClientJSON "./Data/Clients.json" client


getNewClient :: IO Client
getNewClient = do
  userName <- getName
  userAge <- getAge
  userCPF <- getCPF
  userEmail <- getEmail
  userPassword <- getPassword
  return $ createClient 10 userName userAge userCPF userEmail userPassword 100.00