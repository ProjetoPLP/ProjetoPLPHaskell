{-# LANGUAGE DeriveGeneric #-}

module CreatePeople where

import GHC.Generics
import GetInfoForCreatePeople

data People = People {
    identifier :: Int,
    name :: String,
    age :: Int,
    cpf :: Int,
    email :: String,
    password :: Int,
    cash :: Int
} deriving (Show, Generic)

getPeople :: IO People
getPeople = do
  userName <- getName
  userAge <- getAge
  userCPF <- getCPF
  userEmail <- getEmail
  userPassword <- getPassword
  return $
    People { 
        identifier = userCPF,
        name = userName,
        age = userAge,
        cpf = userCPF,
        email = userEmail,
        password = userPassword,
        cash = 0
      }
