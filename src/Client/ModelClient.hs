{-# LANGUAGE DeriveGeneric #-}
module Client.ModelClient where
import GHC.Generics

data Client = Client
  { ident :: Int,
    name :: String,
    age :: Int,
    cpf :: Int,
    email :: String,
    password :: Int,
    cash :: Float,
    assets :: Float,
    canDeposit :: Bool,
    row :: Int,
    col :: Int,
    allAssets :: [Asset]
  }
  deriving (Show, Generic)

data Asset = Asset {
  companyID :: Int,
  price :: Float
} deriving (Show, Generic)

createClient :: Int -> String -> Int -> Int -> String -> Int -> Float -> Client
createClient id name age cpf email password cash =
  Client
    { ident = id,
      name = name,
      age = age,
      cpf = cpf,
      email = email,
      password = password,
      cash = cash,
      assets = 0.00,
      canDeposit = False,
      row = 19,
      col = 52,
      allAssets = []
    }

createAsset :: Int -> Float -> Asset
createAsset companyID price = 
  Asset {
    companyID = companyID,
    price = price
  }
