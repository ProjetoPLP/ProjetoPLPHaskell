module Main where

import CreateClient
import ModelClient
import SaveClient

main :: IO ()
main = do
  questionClient <- getClient
  let createNewClient =
        createClient
          (identifier questionClient)
          (name questionClient)
          (age questionClient)
          (cpf questionClient)
          (email questionClient)
          (password questionClient)
          (cash questionClient)
  print (questionClient)
  print (createNewClient)
  saveClientJSON "./Temp.json" createNewClient
