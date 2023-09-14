{-# LANGUAGE DeriveGeneric #-}
module Models.Client.ModelClient where
import GHC.Generics

data Client = Client
  { ident :: Int,
    name :: String,
    age :: String,
    cpf :: String,
    email :: String,
    password :: String,
    cash :: Float,
    patrimony :: Float,
    trendIndicator :: String,
    canDeposit :: Bool,
    row :: Int,
    col :: Int,
    allAssets :: [Asset]
  }
  deriving (Show, Generic)

data Asset = Asset {
  companyID :: Int,
  qtd :: Int
} deriving (Show, Generic)

createClient :: Int -> String -> String -> String -> String -> String -> Float -> Client
createClient id_ name age cpf email password cash =
  Client
    { ident = id_,
      name = name,
      age = age,
      cpf = cpf,
      email = email,
      password = password,
      cash = cash,
      patrimony = 0,
      trendIndicator = " ",
      canDeposit = False,
      row = 19,
      col = 51,
      allAssets = []
    }

createAsset :: Int -> Int -> Asset
createAsset companyID qtd = 
  Asset {
    companyID = companyID,
    qtd = qtd
  }
