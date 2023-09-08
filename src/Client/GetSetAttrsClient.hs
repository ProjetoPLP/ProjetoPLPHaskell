module Client.GetSetAttrsClient where
import Client.SaveClient
import Client.ModelClient

-- ================================ GetAttributeClient ============================= --

-- ====================== getNameOfClient ============================ --
-- Entrada: id: Int
-- TipoDeSaida: String
getName :: Int -> Maybe String
getName id = do
  let clientName = name (getClient id)
  if clientName == ""
    then Nothing
    else Just clientName

-- ====================== getNameOfClient ============================ --
-- Entrada: id: Int
-- TipoDeSaida: Int
getAge :: Int -> Maybe Int
getAge id = do
  let clientAge = age (getClient id)
  if clientAge == 0
    then Nothing
    else Just clientAge

-- ====================== getCPFOfClient ============================= --
-- Entrada: id: Int
-- TipoDeSaida: String
getCPF :: Int -> Maybe String
getCPF id = do
  let clientCPF = cpf (getClient id)
  if clientCPF == 0
    then Nothing
    else Just (formatCPF(clientCPF))

-- ====================== getEmailOfClient =========================== --
-- Entrada: id: Int
-- TipoDeSaida: String
getEmail :: Int -> Maybe String
getEmail id = do
  let clientEmail = email (getClient id)
  if clientEmail == ""
    then Nothing
    else Just clientEmail

-- ====================== getPasswordOfClient ======================== --
-- Entrada: id: Int
-- TipoDeSaida: Int
getPassword :: Int -> Maybe Int
getPassword id = do
  let clientPassword = password (getClient id)
  if clientPassword == 0
    then Nothing
    else Just clientPassword

-- ====================== getCashOfClient ============================ --
-- Entrada: id: Int
-- TipoDeSaida: Float
getCash :: Int -> Float
getCash id = cash (getClient id)

-- ====================== getAssetsOfClient ========================== --
-- Entrada: id: Int
-- TipoDeSaida: Float
getPatrimony :: Int -> Float
getPatrimony id = patrimony (getClient id)

-- ====================== getCanDepositOfClient ====================== --
-- Entrada: id: Int
-- TipoDeSaida: Bool
getCanDeposit :: Int -> Bool
getCanDeposit id = canDeposit (getClient id)

-- ====================== getRowOfClient ============================= --
-- Entrada: id: Int
-- TipoDeSaida: Int
getRow :: Int -> Int
getRow id = row (getClient id)

-- ====================== getColOfClient ============================= --
-- Entrada: id: Int
-- TipoDeSaida: Int
getCol :: Int -> Int
getCol id = col (getClient id)

-- ====================== getAllAssetsOfClient ============================= --
-- Entrada: id: Int
-- TipoDeSaida: [Asset]
getAllAssets :: Int -> [Asset]
getAllAssets id = allAssets (getClient id)

-- ================================ SetAttributeClient ============================= --

-- ====================== setNameOfClient ============================ --
-- Entrada: id: Int / name: String
-- TipoDeSaida: Bool
setName :: Int -> String -> IO Bool
setName id name = do 
    let client = getClient id
    if (ident client) /= -1 then do
        let newClient = client { name = name }
        editClientJSON "./Data/Clients.json" newClient
        return True
    else do
        putStrLn "Cliente não existente."
        return False

-- ====================== setAgeOfClient ============================ --
-- Entrada: id: Int / age: Int
-- TipoDeSaida: Bool
setAge :: Int -> Int -> IO Bool
setAge id age = do
    let client = getClient id
    if (age >= 18) then do
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

-- ====================== setCPFOfClient =========================== --
-- Entrada: id: Int / cpf: Int
-- TipoDeSaida: Bool
setCPF :: Int -> Int -> IO Bool
setCPF id cpf = do
    let client = getClient id
    if (length (show cpf) == 11) then do
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

-- ====================== setEmailOfClient ========================= --
-- Entrada: id: Int / email: String
-- TipoDeSaida: Bool
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

-- ====================== setPasswordOfClient ====================== --
-- Entrada: id: Int / password: Int
-- TipoDeSaida: Bool
setPassword :: Int -> Int -> IO Bool
setPassword id password = do
    let client = getClient id
    if (length (show password) == 5) then do
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

-- ====================== setCashOfClient ========================== --
-- Entrada: id: Int / cash: Float
-- TipoDeSaida: Bool
setCash :: Int -> Float -> IO Bool
setCash id cash = do
    let client = getClient id
    if (ident client) /= -1 then do
        let newClient = client { cash = cash }
        editClientJSON "./Data/Clients.json" newClient
        return True
    else do
        putStrLn "\nOcorreu um problema! O Cliente com este id não foi encontrado!"
        return False

-- ====================== setPatrimonyOfClient ========================== --
-- Entrada: id: Int / patrimony: Float
-- TipoDeSaida: Bool
setPatrimony :: Int -> Float -> IO Bool
setPatrimony id patrimony = do
    let client = getClient id
    if (ident client) /= -1 then do
        let newClient = client { patrimony = patrimony }
        editClientJSON "./Data/Clients.json" newClient
        return True
    else do
        putStrLn "\nOcorreu um problema! O Cliente com este id não foi encontrado!"
        return False

-- ====================== setCanDepositOfClient =================== --
-- Entrada: id: Int / canDeposit: Bool
-- TipoDeSaida: Bool
setCanDeposit :: Int -> Bool -> IO Bool
setCanDeposit id canDeposit = do
    let client = getClient id
    if (ident client) /= -1 then do
        let newClient = client { canDeposit = canDeposit }
        editClientJSON "./Data/Clients.json" newClient
        return True
    else do
        putStrLn "\nOcorreu um problema! O Cliente com este id não foi encontrado!"
        return False

-- ====================== setRowOfClient ========================== --
-- Entrada: id: Int / row: Int
-- TipoDeSaida: Bool
setRow :: Int -> Int -> IO Bool
setRow id row = do
    let client = getClient id
    if (ident client) /= -1 then do
        let newClient = client { row = row }
        editClientJSON "./Data/Clients.json" newClient
        return True
    else do
        putStrLn "\nOcorreu um problema! O Cliente com este id não foi encontrado!"
        return False

-- ====================== setColOfClient ========================== --
-- Entrada: id: Int / col: Int
-- TipoDeSaida: Bool
setCol :: Int -> Int -> IO Bool
setCol id col = do
    let client = getClient id
    if (ident client) /= -1 then do
        let newClient = client { col = col }
        editClientJSON "./Data/Clients.json" newClient
        return True
    else do
        putStrLn "\nOcorreu um problema! O Cliente com este id não foi encontrado!"
        return False

-- ====================== setAllAssetsOfClient ========================== --
-- Entrada: id: Int / allAssets: [Asset]
-- TipoDeSaida: Bool
setAllAssets :: Int -> [Asset] -> IO Bool
setAllAssets id allAssets = do
    let client = getClient id
    if (ident client) /= -1 then do
        let newClient = client { allAssets = allAssets }
        editClientJSON "./Data/Clients.json" newClient
        return True
    else do
        putStrLn "\nOcorreu um problema! O Cliente com este id não foi encontrado!"
        return False

-- ================================ OthersMethodsAux ================================= --

-- ====================== formatCPF =========================== --
-- Entrada: cpf: Int
-- TipoDeSaida: Saida
formatCPF :: Int -> String
formatCPF cpf =
  let cpfStr = show cpf
  in if length cpfStr == 11
       then
         let part1 = take 3 cpfStr
             part2 = take 3 (drop 3 cpfStr)
             part3 = take 3 (drop 6 cpfStr)
             part4 = drop 9 cpfStr
         in part1 ++ "." ++ part2 ++ "." ++ part3 ++ "-" ++ part4
       else "CPF inválido"

-- ====================== addCashClient: Remover em breve ===== --
-- Entrada: id: Int / cash: Float
-- TipoDeSaida: None
addCash :: Int -> Float -> IO()
addCash id cashAdd = do
    let client = getClientsByID id (getClientJSON "./Data/Clients.json")
    let newCash = fromIntegral (round ((cash client + cashAdd) * 10)) / 10
    let newClient = client {cash = newCash}
    editClientJSON "../Data/Clients.json" newClient
