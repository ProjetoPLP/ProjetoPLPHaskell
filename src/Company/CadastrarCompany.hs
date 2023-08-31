module CadastrarCompany where

import SaveCompany
import ModelCompany
import CreateCompany

cadastrarCompany :: IO String
cadastrarCompany = do
    company <- getCompany
    if length (show (password company)) == 5 then do
        if not (existCompanyByName (name company)) then do
            saveCompanyJSON "../Data/Companies.json" company
            return ("\nParabéns, a empresa " ++ name company ++ " foi cadastrada com sucesso!")
        else return "\nOcorreu um problema! A Empresa já está cadastrada!"
    else  
        return "\nOcorreu um problema! A senha deve ter 5 digitos."
