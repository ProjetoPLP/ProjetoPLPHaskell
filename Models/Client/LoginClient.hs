module Models.Client.LoginClient where

import System.IO ( withFile, IOMode(WriteMode) )
import System.Directory ( removeFile, renameFile )

import Data.Aeson ( FromJSON, ToJSON, eitherDecodeFileStrict, encode, decode )
import Data.Maybe ( isJust )
import qualified Data.ByteString.Lazy as B

import Models.Client.ModelClient ( Client(Client, email, ident), Asset )

instance FromJSON Client
instance ToJSON Client
instance FromJSON Asset
instance ToJSON Asset


saveLogin :: Client -> IO ()
saveLogin client = do
  B.writeFile jsonFilePath $ encode client
  where jsonFilePath = "./Data/Login.json"


logoutClient :: IO ()
logoutClient = writeFile "./Data/Login.json" ""


-- Ler cliente logado
getLoggedClient :: IO Client
getLoggedClient = do
    result <- readClientFromFile "./Data/Login.json"
    case result of
        Left _ -> return (Client (-1) "" "" "" "" "" 0 0 " " False 19 52 [])
        Right client -> return client


readClientFromFile :: FilePath -> IO (Either String Client)
readClientFromFile = eitherDecodeFileStrict