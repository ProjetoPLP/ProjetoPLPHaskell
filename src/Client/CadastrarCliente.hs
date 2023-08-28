module CadastrarCliente where

import SaveClient
import ModelClient
import CreateClient

cadastrarCliente :: IO String
cadastrarCliente = do
  client <- getClient
  if (age client) >= 18 then do
    saveClientJSON "./Temp.json" client
    return (name client ++ " cadastrado! Você iniciará com um saldo de R$100,00")
  else
    return "Proibido menores de 18 anos."
