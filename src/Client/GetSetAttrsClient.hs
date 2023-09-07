module Client.GetSetAttrsClient where
import Client.SaveClient
import Client.ModelClient

-- ================================ GetAttributeClient ============================= --
getName :: Int -> Maybe String
getName id = do
  let clientName = name (getClient id)
  if clientName == ""
    then Nothing
    else Just clientName

getAge :: Int -> Maybe Int
getAge id = do
  let clientAge = age (getClient id)
  if clientAge == 0
    then Nothing
    else Just clientAge

getCPF :: Int -> Maybe String
getCPF id = do
  let clientCPF = cpf (getClient id)
  if clientCPF == 0
    then Nothing
    else Just (formatCPF(clientCPF))

getEmail :: Int -> Maybe String
getEmail id = do
  let clientEmail = email (getClient id)
  if clientEmail == ""
    then Nothing
    else Just clientEmail

getPassword :: Int -> Maybe Int
getPassword id = do
  let clientPassword = password (getClient id)
  if clientPassword == 0
    then Nothing
    else Just clientPassword

getCash :: Int -> Maybe Float
getCash id = do
  let clientCash = cash (getClient id)
  if clientCash == 0
    then Nothing
    else Just clientCash

getAssets :: Int -> Float
getAssets id = assets (getClient id)

getCanDeposit :: Int -> Bool
getCanDeposit id = canDeposit (getClient id)

getRow :: Int -> Int
getRow id = row (getClient id)

getCol :: Int -> Int
getCol id = col (getClient id)

-- ================================ SetAttributeClient ============================= --
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

setAssets :: Int -> Float -> IO Bool
setAssets id assets = do
    let client = getClient id
    if (ident client) /= -1 then do
        let newClient = client { assets = assets }
        editClientJSON "./Data/Clients.json" newClient
        return True
    else do
        putStrLn "\nOcorreu um problema! O Cliente com este id não foi encontrado!"
        return False

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

-- ================================ OthersAttributes============================= --
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

addCash :: Int -> Float -> IO()
addCash id cashAdd = do
    let client = getClientsByID id (getClientJSON "./Data/Clients.json")
    let newCash = fromIntegral (round ((cash client + cashAdd) * 10)) / 10
    let newClient = client {cash = newCash}
    editClientJSON "../Data/Clients.json" newClient
