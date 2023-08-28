module CreateClient where

import GetInfoForCreateClient
import ModelClient

getClient :: IO Client
getClient = do
  userName <- getName
  userAge <- getAge
  userCPF <- getCPF
  userEmail <- getEmail
  userPassword <- getPassword
  return $ createClient 10 userName userAge userCPF userEmail userPassword 100
