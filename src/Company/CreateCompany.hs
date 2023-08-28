module CreateCompany where

import GetInfoForCreateCompany
import ModelCompany

getCompany :: IO Company
getCompany = do
  companyName <- getName
  companyAgeFounded <- getAgeFounded
  companyCNPJ <- getCNPJ
  companyActuation <- getActuation
  companyDeclaration <- getDeclaration
  companyPassword <- getPassword
  return $ createCompany 10 companyName companyAgeFounded companyCNPJ companyActuation companyDeclaration companyPassword 30.0
