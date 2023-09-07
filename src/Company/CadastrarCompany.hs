module Company.CadastrarCompany where

import Company.SaveCompany
import Company.ModelCompany
import Company.CreateCompany

cadastrarCompany :: IO Bool
cadastrarCompany = do
    company <- getCompany
    if (length (show (cnpj company))) == 14 then do
        if length (show (code company)) == 5 then do
            if not (existCompanyByName (name company)) then do
                saveCompanyJSON "./Data/Companies.json" company
                putStrLn ("\nParabéns, a empresa " ++ name company ++ " foi cadastrada com sucesso!")
                return True
            else do 
                putStrLn "\nOcorreu um problema! A Empresa já está cadastrada!"
                return False
        else do 
            putStrLn "\nOcorreu um problema! A senha deve ter 5 digitos."
            return False
    else do
        putStrLn "\nOcorreu um problema! O CNPJ não contém 14 digitos."
        return False
