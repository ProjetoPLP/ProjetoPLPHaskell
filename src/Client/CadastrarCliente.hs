module CadastrarCliente where

import SaveClient
import ModelClient
import CreateClient

cadastrarCliente :: IO String
cadastrarCliente = do
    client <- getClient
    if (age client) >= 18 then do
        if (length (show (cpf client))) == 11 then do
            if (length (show (password client))) == 5 then do 
                if not (existClientByEmail (email client)) then do
                    saveClientJSON "../Data/Clients.json" client
                    return ("\n" ++ name client ++ " você foi cadastrado! Você iniciará com um saldo de R$100,00.")
                else return "\nOcorreu um problema! O Cliente já está cadastrado!"
            else return "\nOcorreu um problema! A senha deve ter 5 digitos."
        else return "\nOcorreu um problema! O CPF não contém 11 dígitos."
    else return "\nOcorreu um problema! Proibido menores de 18 anos."
