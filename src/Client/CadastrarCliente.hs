module Client.CadastrarCliente where

import Client.SaveClient
import Client.ModelClient
import Client.CreateClient

cadastrarCliente :: IO Bool
cadastrarCliente = do
    client <- getNewClient
    if length (name client) <= 18 then
        if age client >= 18 then do
            if length (cpf client) == 11 then do
                if length (show (password client)) >= 5 then do
                    if not (existClientByEmail (email client)) then do
                        saveClientJSON "./Data/Clients.json" client
                        return True
                    else do
                        putStrLn "Aviso: O e-mail já foi cadastrado."
                        return False
                else do
                    putStrLn "Aviso: A senha deve ter no mínimo 5 digitos."
                    return False
            else do
                putStrLn "Aviso: O CPF não contém 11 dígitos."
                return False
        else do
            putStrLn "Aviso: Proibido menores de 18 anos."
            return False
    else do
        putStrLn "Aviso: O nome do usuário deve ter no máximo 18 caracteres."
        return False
