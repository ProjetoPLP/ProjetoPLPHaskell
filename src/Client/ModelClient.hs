{-# LANGUAGE DeriveGeneric #-}
module ModelClient where
import GHC.Generics

data Client = Client
  { identifier :: Int,
    name :: String,
    age :: Int,
    cpf :: Int,
    email :: String,
    password :: Int,
    cash :: Int
  }
  deriving (Show, Generic)

createClient :: Int -> String -> Int -> Int -> String -> Int -> Int -> Client
createClient identifier name age cpf email password cash =
  Client
    { identifier = identifier,
      name = name,
      age = age,
      cpf = cpf,
      email = email,
      password = password,
      cash = cash
    }
