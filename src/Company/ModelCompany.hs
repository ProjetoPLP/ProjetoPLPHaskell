{-# LANGUAGE DeriveGeneric #-}
module Company.ModelCompany where
import GHC.Generics

data Company = Company
  { identifier :: Int,
    name :: String,
    ageFounded :: Int,
    cnpj :: Int,
    actuation :: String,
    declaration :: String,
    code :: Int,
    actions :: Float
  }
  deriving (Show, Generic)

createCompany :: Int -> String -> Int -> Int -> String -> String -> Int -> Float -> Company
createCompany identifier name ageFounded cnpj actuation declaration code actions =
  Company
    { identifier = identifier,
      name = name,
      ageFounded = ageFounded,
      cnpj = cnpj,
      actuation = actuation,
      declaration = declaration,
      code = code,
      actions = actions
    }
