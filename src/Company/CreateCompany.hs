module Company.CreateCompany where

import Company.GetInfoForCreateCompany
import Company.ModelCompany

import System.Random (randomRIO)

getNewCompany :: IO Company
getNewCompany = do
  companyName <- getName
  companyAgeFounded <- getAgeFounded
  companyCNPJ <- getCNPJ
  companyActuation <- getActuation
  companyDeclaration <- getDeclaration
  companyCode <- getCode
  companyPrice <- randomCompanyPrice
  return $ createCompany 10 companyName companyAgeFounded companyCNPJ companyActuation companyDeclaration companyCode companyPrice " "


-- Retorna um preço aleatório entre 10.0 e 30.0 
randomCompanyPrice :: IO Float
randomCompanyPrice = do
    newPrice <- randomRIO (10,30 :: Float)
    return (format newPrice)
    where
        format :: Float -> Float
        format newPrice = fromIntegral (round (newPrice * 10 )) / 10