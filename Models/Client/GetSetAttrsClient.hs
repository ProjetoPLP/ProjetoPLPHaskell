module Models.Client.GetSetAttrsClient where

import Models.Client.SaveClient ( editClientJSON, getClient, getClientJSON, getClientsByID )
import Models.Client.ModelClient ( Asset(qtd, companyID), Client(ident, name, age, cpf, email, password, patrimony, canDeposit, row, col, allAssets, cash) )
import Models.Client.LoginClient ( getLoggedClient )


getLoggedUserID :: IO Int
getLoggedUserID = ident <$> getLoggedClient


getName :: Int -> String
getName idUser = name (getClient idUser)


getCPF :: Int -> String
getCPF idUser = cpf (getClient idUser)


getCash :: Int -> Float
getCash idUser = cash (getClient idUser)


getPatrimony :: Int -> Float
getPatrimony idUser = patrimony (getClient idUser)


getCanDeposit :: Int -> Bool
getCanDeposit idUser = canDeposit (getClient idUser)


getRow :: Int -> Int
getRow idUser = row (getClient idUser)


getCol :: Int -> Int
getCol idUser = col (getClient idUser)


-- Retorna uma lista com as empresas nas quais o usuário possui ações
getAllAssets :: Int -> [Asset]
getAllAssets idUser = allAssets (getClient idUser)


-- Retorna a quantidade de ações que um cliente X possui em uma empresa Y
getQtdAssetsInCompany :: Int -> Int -> Int
getQtdAssetsInCompany idClient = getQtdAssetsInCompanyAux (getAllAssets idClient)
    where
        getQtdAssetsInCompanyAux :: [Asset] -> Int -> Int
        getQtdAssetsInCompanyAux [] idComp = 0
        getQtdAssetsInCompanyAux (x:xs) idComp =
            if companyID x == idComp then qtd x
            else getQtdAssetsInCompanyAux xs idComp


setCash :: Int -> Float -> IO ()
setCash idUser cash = do
    let client = getClient idUser
        newClient = client { cash = cash }
    editClientJSON "./Data/Clients.json" newClient


setPatrimony :: Int -> Float -> IO ()
setPatrimony idUser patrimony = do
    let client = getClient idUser
        newClient = client { patrimony = patrimony }
    editClientJSON "./Data/Clients.json" newClient


setCanDeposit :: Int -> Bool -> IO ()
setCanDeposit idUser canDeposit = do
    let client = getClient idUser
        newClient = client { canDeposit = canDeposit }
    editClientJSON "./Data/Clients.json" newClient


setAllAssets :: Int -> [Asset] -> IO ()
setAllAssets idUser allAssets = do
    let client = getClient idUser
        newClient = client { allAssets = allAssets }
    editClientJSON "./Data/Clients.json" newClient


setRow :: Int -> Int -> IO ()
setRow idUser row = do
    let client = getClient idUser
        newClient = client { row = row }
    editClientJSON "./Data/Clients.json" newClient


setCol :: Int -> Int -> IO ()
setCol idUser col = do
    let client = getClient idUser
        newClient = client { col = col }
    editClientJSON "./Data/Clients.json" newClient


addCash :: Int -> Float -> IO ()
addCash idUser cashAdd = do
    let client = getClientsByID idUser (getClientJSON "./Data/Clients.json")
        newCash = fromIntegral (round ((cash client + cashAdd) * 10)) / 10
        newClient = client {cash = newCash}
    editClientJSON "./Data/Clients.json" newClient


addRow :: Int -> Int -> IO ()
addRow idUser row = do
    let client = getClientsByID idUser (getClientJSON "./Data/Clients.json")
        newClient = client {row = getRow idUser + row}
    editClientJSON "./Data/Clients.json" newClient


addCol :: Int -> Int -> IO ()
addCol idUser col = do
    let client = getClientsByID idUser (getClientJSON "./Data/Clients.json")
        newClient = client {col = getCol idUser + col}
    editClientJSON "./Data/Clients.json" newClient