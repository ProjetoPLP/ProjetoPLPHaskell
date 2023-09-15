module Models.Client.CadastrarCliente where

import Models.Client.SaveClient ( saveClientJSON )
import Models.Client.ModelClient ( createClient, Client )
import Models.Client.GetInfoForCreateClient ( getName, getAge, getCPF, getEmail, getPassword )


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