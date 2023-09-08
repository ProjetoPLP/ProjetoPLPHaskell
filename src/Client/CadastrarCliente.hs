module Client.CadastrarCliente where

import Client.SaveClient
import Client.ModelClient
import Client.CreateClient

cadastrarCliente :: IO Bool
cadastrarCliente = do
    client <- getNewClient
    if (length (name client) <= 18) then
        if (age client) >= 18 then do
            if (length (show (cpf client))) == 11 then do
                if (length (show (password client))) == 5 then do 
                    if not (existClientByEmail (email client)) then do
                        saveClientJSON "./Data/Clients.json" client
                        putStrLn ("\n" ++ name client ++ " você foi cadastrado! Você iniciará com um saldo de R$100,00.")
                        return True
                    else do
                        putStrLn "\nOcorreu um problema! O Cliente já está cadastrado!"
                        return False
                else do
                    putStrLn "\nOcorreu um problema! A senha deve ter 5 digitos."
                    return False
            else do
                putStrLn "\nOcorreu um problema! O CPF não contém 11 dígitos."
                return False
        else do
            putStrLn "\nOcorreu um problema! Proibido menores de 18 anos."
            return False
    else do
        putStrLn "\nOcorreu um problema! O nome do cliente deve ter no máximo 18 caracteres."
        return False
