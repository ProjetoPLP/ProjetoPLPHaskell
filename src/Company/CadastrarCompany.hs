module Company.CadastrarCompany where

import Company.SaveCompany
import Company.ModelCompany
import Company.CreateCompany

cadastrarCompany :: IO Bool
cadastrarCompany = do
    company <- getNewCompany
    if (length (name company) <= 18) then
        if (length (actuation company) <= 15) then do
            if (length (declaration company) <= 86) then do
                if (length (cnpj company)) == 14 then do
                    if not (existCompanyByName (name company)) then do
                        if length (code company) == 5 then do
                            saveCompanyJSON "./Data/Companies.json" company
                            putStrLn ("\nParabéns, a empresa " ++ name company ++ " foi cadastrada com sucesso!")
                            return True
                        else do
                            putStrLn "\nOcorreu um problema! O código da Empresa deve ter 5 caracteres!"
                            return False
                    else do 
                        putStrLn "\nOcorreu um problema! A Empresa já está cadastrada!"
                        return False
                else do
                    putStrLn "\nOcorreu um problema! O CNPJ não contém 14 digitos."
                    return False
            else do
                putStrLn "\nOcorreu um problema! A área de atuação da Empresa deve ter no máximo 86 caracteres!"
                return False
        else do
            putStrLn "\nOcorreu um problema! A declaração da Empresa deve ter no máximo 15 caracteres!"
            return False
    else do
        putStrLn "\nOcorreu um problema! O nome da empresa deve ter no máximo 18 caracteres."
        return False
