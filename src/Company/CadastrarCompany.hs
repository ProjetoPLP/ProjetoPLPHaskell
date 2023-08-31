module CadastrarCompany where

import SaveCompany
import ModelCompany
import CreateCompany

cadastrarCompany :: IO String
cadastrarCompany = do
  company <- getCompany
  saveCompanyJSON "./Companies.json" company
  return ("A empresa " ++ name company ++ " foi cadastrada com sucesso!")
