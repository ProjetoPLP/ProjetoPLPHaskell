module Utils.VerificationUtils where

import Company.ModelCompany (Company)
import Company.GetSetAttrsCompany (getIdent)
import Company.SaveCompany (getCompanyJSON)


-- Verifica se existe uma empresa cadastrada a bolsa a partir do seu ID
existCompany :: Int -> Bool
existCompany id = existCompanyAux id (getCompanyJSON "./Data/Companies.json")


existCompanyAux :: Int -> [Company] -> Bool
existCompanyAux _ [] = False
existCompanyAux id (x:xs) = (getIdent x == id) || existCompanyAux id xs
