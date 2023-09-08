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
    trendIndicator :: Char,
    minPrice ::Float,
    maxPrice :: Float,
    startPrice :: Float,
    row :: Int,
    col :: Int
  }
  deriving (Show, Generic)

createCompany :: Int -> String -> Int -> Int -> String -> String -> String -> Float -> Char -> Float -> Float -> Float -> Company
createCompany id_ name age cnpj actuation declaration code price trendIndicator minPrice maxPrice startPrice =
  Company
    { ident = id_,
      name = name,
      age = age,
      cnpj = cnpj,
      actuation = actuation,
      declaration = declaration,
      code = code,
      price = price,
      trendIndicator = trendIndicator,
      row = 20,
      col = 3,
      minPrice = minPrice,
      maxPrice = maxPrice,
      startPrice = startPrice
    }
