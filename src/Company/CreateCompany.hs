module Client.CreateCompany where

import Client.GetInfoForCreateCompany
import Client.ModelCompany

getCompany :: IO Company
getCompany = do
  companyName <- getName
  companyAgeFounded <- getAgeFounded
  companyCNPJ <- getCNPJ
  companyActuation <- getActuation
  companyDeclaration <- getDeclaration
  companyCode <- getCode
  return $ createCompany 10 companyName companyAgeFounded companyCNPJ companyActuation companyDeclaration companyCode 30.0
