{-# LANGUAGE DeriveGeneric #-}
module ModelCompany where
import GHC.Generics

data Company = Company
  { identifier :: Int,
    name :: String,
    ageFounded :: Int,
    cnpj :: Int,
    actuation :: String,
    declaration :: String,
    password :: Int,
    actions :: Double
  }
  deriving (Show, Generic)

createCompany :: Int -> String -> Int -> Int -> String -> String -> Int -> Double -> Company
createCompany identifier name ageFounded cnpj actuation declaration password actions =
  Company
    { identifier = identifier,
      name = name,
      ageFounded = ageFounded,
      cnpj = cnpj,
      actuation = actuation,
      declaration = declaration,
      password = password,
      actions = actions
    }
