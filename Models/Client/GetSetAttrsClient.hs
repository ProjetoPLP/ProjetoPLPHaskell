module Models.Client.GetSetAttrsClient where

import Models.Client.SaveClient ( editClientJSON, getClient, getClientJSON, getClientsByID )
import Models.Client.ModelClient ( Asset(qtd, companyID), Client(ident, name, age, cpf, email, password, patrimony, trendIndicator, canDeposit, row, col, allAssets, cash) )
import Models.Client.LoginClient ( getLoggedClient )


getName :: Int -> String
getName id = name (getClient id)


getAge :: Int -> String
getAge id = age (getClient id)


getCPF :: Int -> String
getCPF id = formatCPF (cpf (getClient id))


getEmail :: Int -> String
getEmail id = email (getClient id)


getPassword :: Int -> String
getPassword id = password (getClient id)


getCash :: Int -> Float
getCash id = cash (getClient id)


addCash :: Int -> Float -> IO ()
addCash id cashAdd = do
    let client = getClientsByID id (getClientJSON "./Data/Clients.json")
        newCash = fromIntegral (round ((cash client + cashAdd) * 10)) / 10
        newClient = client {cash = newCash}
    editClientJSON "./Data/Clients.json" newClient


getPatrimony :: Int -> Float
getPatrimony id = patrimony (getClient id)


getTrendIndicator :: Int -> String
getTrendIndicator id = trendIndicator (getClient id)


getCanDeposit :: Int -> Bool
getCanDeposit id = canDeposit (getClient id)


getRow :: Int -> Int
getRow id = row (getClient id)


getCol :: Int -> Int
getCol id = col (getClient id)


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


-- Retorna uma lista com as empresas nas quais o usuário possui ações
getAllAssets :: Int -> [Asset]
getAllAssets id = allAssets (getClient id)


-- Retorna a quantidade de ações que um cliente X possui em uma empresa Y
getQtdAssetsInCompany :: Int -> Int -> Int
getQtdAssetsInCompany idClient = getQtdAssetsInCompanyAux (getAllAssets idClient)
    where
        getQtdAssetsInCompanyAux :: [Asset] -> Int -> Int
        getQtdAssetsInCompanyAux [] idComp = 0
        getQtdAssetsInCompanyAux (x:xs) idComp =
            if companyID x == idComp then qtd x
            else getQtdAssetsInCompanyAux xs idComp


setName :: Int -> String -> IO Bool
setName id name = do
    let client = getClient id
    if (length name) <= 18 then do
        if (ident client) /= -1 then do
            let newClient = client { name = name }
            editClientJSON "./Data/Clients.json" newClient
            return True
        else do
            putStrLn "Cliente não existente."
            return False
    else do
        putStrLn "\nOcorreu um problema! O nome do cliente deve ter no máximo 18 caracteres."
        return False


setAge :: Int -> String -> IO Bool
setAge id age = do
    let client = getClient id
    if (read age :: Int) >= 18 then do
        if (ident client) /= -1 then do
            let newClient = client { age = age }
            editClientJSON "./Data/Clients.json" newClient
            return True
        else do
            putStrLn "\nOcorreu um problema! O Cliente com este id não foi encontrado!"
            return False
    else do
        putStrLn "\nOcorreu um problema! Proibido menores de 18 anos."
        return False


setCPF :: Int -> String -> IO Bool
setCPF id cpf = do
    let client = getClient id
    if (length cpf) == 11 then do
        if (ident client) /= -1 then do
            let newClient = client { cpf = cpf }
            editClientJSON "./Data/Clients.json" newClient
            return True
        else do
            putStrLn "\nOcorreu um problema! O Cliente com este id não foi encontrado!"
            return False
    else do
        putStrLn "\nOcorreu um problema! O CPF não contém 11 dígitos."
        return False


setEmail :: Int -> String -> IO Bool
setEmail id email = do
    let client = getClient id
    if (ident client) /= -1 then do
        let newClient = client { email = email }
        editClientJSON "./Data/Clients.json" newClient
        return True
    else do
        putStrLn "\nOcorreu um problema! O Cliente com este id não foi encontrado!"
        return False


setPassword :: Int -> String -> IO Bool
setPassword id password = do
    let client = getClient id
    if (length password) == 5 then do
        if (ident client) /= -1 then do
            let newClient = client { password = password }
            editClientJSON "./Data/Clients.json" newClient
            return True
        else do
            putStrLn "\nOcorreu um problema! O Cliente com este id não foi encontrado!"
            return False
    else do
        putStrLn "\nOcorreu um problema! A senha deve ter 5 digitos."
        return False


setCash :: Int -> Float -> IO ()
setCash id cash = do
    let client = getClient id
    if (ident client) /= -1 then do
        let newClient = client { cash = cash }
        editClientJSON "./Data/Clients.json" newClient
    else do
        putStrLn "\nOcorreu um problema! O Cliente com este id não foi encontrado!"


setPatrimony :: Int -> Float -> IO ()
setPatrimony id patrimony = do
    let client = getClient id
    if (ident client) /= -1 then do
        let newClient = client { patrimony = patrimony }
        editClientJSON "./Data/Clients.json" newClient
    else do
        putStrLn "\nOcorreu um problema! O Cliente com este id não foi encontrado!"


setTrendIndicator :: Int -> String -> IO ()
setTrendIndicator id trendIndicator = do
    let client = getClient id
    if ident client /= -1 then do
        let newClient = client { trendIndicator = trendIndicator }
        editClientJSON "./Data/Clients.json" newClient
    else do
        putStrLn "\nOcorreu um problema! O Cliente com este id não foi encontrado!"


setCanDeposit :: Int -> Bool -> IO ()
setCanDeposit id canDeposit = do
    let client = getClient id
    if (ident client) /= -1 then do
        let newClient = client { canDeposit = canDeposit }
        editClientJSON "./Data/Clients.json" newClient
    else do
        putStrLn "\nOcorreu um problema! O Cliente com este id não foi encontrado!"


setAllAssets :: Int -> [Asset] -> IO ()
setAllAssets id allAssets = do
    let client = getClient id
    if (ident client) /= -1 then do
        let newClient = client { allAssets = allAssets }
        editClientJSON "./Data/Clients.json" newClient
    else do
        putStrLn "\nOcorreu um problema! O Cliente com este id não foi encontrado!"

-- ================================ OthersMethodsAux ================================= --


formatCPF :: String -> String
formatCPF cpf =
  let cpfStr = cpf
  in if length cpfStr == 11
       then
         let part1 = take 3 cpfStr
             part2 = take 3 (drop 3 cpfStr)
             part3 = take 3 (drop 6 cpfStr)
             part4 = drop 9 cpfStr
         in part1 ++ "." ++ part2 ++ "." ++ part3 ++ "-" ++ part4
       else "CPF inválido"


getLoggedUserID :: IO Int
getLoggedUserID = do
    ident <$> getLoggedClient