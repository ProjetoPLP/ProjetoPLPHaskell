module Company.CadastrarCompany where

import Company.SaveCompany
import Company.ModelCompany
import Company.CreateCompany

cadastrarCompany :: IO Bool
cadastrarCompany = do
    company <- getNewCompany
    if length (name company) <= 18 then
        if length (actuation company) <= 29 then do
            if length (declaration company) <= 86 then do
                if length (cnpj company) == 14 then do
                    if not (existCompanyByName (name company)) then do
                        if length (code company) == 5 then do
                            saveCompanyJSON "./Data/Companies.json" company
                            return True
                        else do
                            putStrLn "Aviso: O código da empresa deve possuir 5 caracteres."
                            return False
                    else do
                        putStrLn "Aviso: A empresa já foi cadastrada."
                        return False
                else do
                    putStrLn "Aviso: O CNPJ não contém 14 digitos."
                    return False
            else do
                putStrLn "Aviso: A declaração de missão da empresa deve ter no máximo 86 caracteres."
                return False
        else do
            putStrLn "Aviso: A área de atuação da empresa deve ter no máximo 29 caracteres."
            return False
    else do
        putStrLn "Aviso: O nome da empresa deve ter no máximo 18 caracteres."
        return False
