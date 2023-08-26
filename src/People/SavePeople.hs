{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module SavePeople where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import GHC.Generics
import System.IO.Unsafe
import System.IO
import System.Directory

data People = People {
    identifier :: Int,
    name :: String,
    age :: Int,
    cpf :: Int,
    email :: String,
    password :: Int,
    cash :: Int
} deriving (Show, Generic)

instance FromJSON People
instance ToJSON People

getPeopleJSON :: String -> [People]
getPeopleJSON path = do
  let file = unsafePerformIO ( B.readFile path )
  let decodedFile = decode file :: Maybe [People]
  case decodedFile of
    Nothing -> []
    Just out -> out

savePeopleJSON :: String -> Int -> String -> Int -> Int -> String -> Int -> Int -> IO()
savePeopleJSON jsonFilePath identifier name age cpf email password cash = do
  let p = People identifier name age cpf email password cash
  let peopleList = (getPeopleJSON "../data/people.json") ++ [p]

  B.writeFile "../data/people.json" $ encode peopleList
  removeFile jsonFilePath
  renameFile "../data/people.json" jsonFilePath
