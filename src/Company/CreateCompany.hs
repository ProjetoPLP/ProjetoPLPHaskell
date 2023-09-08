module Company.CreateCompany where

import Company.GetInfoForCreateCompany
import Company.ModelCompany

getNewCompany :: IO Company
getNewCompany = do
  companyName <- getName
  companyAgeFounded <- getAgeFounded
  companyCNPJ <- getCNPJ
  companyActuation <- getActuation
  companyDeclaration <- getDeclaration
  companyCode <- getCode
  return $ createCompany 10 companyName companyAgeFounded companyCNPJ companyActuation companyDeclaration companyCode 30.0 " " 0.00 0.00 0.00
