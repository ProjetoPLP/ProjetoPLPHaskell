module Utils.VerificationUtils where

import Data.Char ( isDigit )
import Models.Company.SaveCompany ( getCompanyJSON )
import Models.Company.ModelCompany ( Company (ident) )


-- Verifica se existe uma empresa cadastrada a bolsa a partir do seu ID
existCompany :: Int -> Bool
existCompany id = existCompanyAux id (getCompanyJSON "./Data/Companies.json")


existCompanyAux :: Int -> [Company] -> Bool
existCompanyAux _ [] = False
existCompanyAux id (x:xs)
    | ident x == id = True
    | otherwise = existCompanyAux id xs


-- Verifica se uma String é um número
isNumber :: String -> Bool
isNumber "" = False
isNumber "0" = False
isNumber userChoice = all isDigit userChoice