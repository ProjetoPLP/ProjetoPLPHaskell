{-# LANGUAGE DeriveGeneric #-}
module Company.ModelCompany where
import GHC.Generics

data Company = Company
  { ident :: Int,
    name :: String,
    age :: Int,
    cnpj :: Int,
    actuation :: String,
    declaration :: String,
    code :: String,
    price :: Float,
    row :: Int,
    col :: Int
  }
  deriving (Show, Generic)

createCompany :: Int -> String -> Int -> Int -> String -> String -> String -> Float -> Company
createCompany id_ name age cnpj actuation declaration code price =
  Company
    { ident = id_,
      name = name,
      age = age,
      cnpj = cnpj,
      actuation = actuation,
      declaration = declaration,
      code = code,
      price = price,
      row = 20,
      col = 3
    }
