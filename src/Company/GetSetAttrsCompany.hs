module Company.GetSetAttrsCompany where
import Company.SaveCompany
import Company.ModelCompany
import Data.Char (toUpper)

getName :: Int -> String
getName id = name (getCompany id)
    
getAge :: Int -> Int
getAge id = age (getCompany id)

getCNPJ :: Int -> String
getCNPJ id = formatCNPJ (cnpj (getCompany id))

getActuation :: Int -> String
getActuation id = actuation (getCompany id)

getDeclaration :: Int -> String
getDeclaration id = declaration (getCompany id)

getCode :: Int -> String
getCode id = code (getCompany id)

getPrice :: Int -> Float
getPrice id = price (getCompany id)

getTrendIndicator :: Int -> String
getTrendIndicator id = trendIndicator (getCompany id)

getMinPrice :: Int -> Float
getMinPrice id = minPrice (getCompany id)

getMaxPrice :: Int -> Float
getMaxPrice id = maxPrice (getCompany id)

getStartPrice :: Int -> Float
getStartPrice id = startPrice (getCompany id)

getRow :: Int -> Int
getRow id = row (getCompany id)

getCol :: Int -> Int
getCol id = col (getCompany id)

getIdent :: Company -> Int
getIdent company = ident company;

-- ==========================

setName :: Int -> String -> IO Bool
setName id name = do
    let company = getCompany id
    if (length name) <= 18 then do
        if (ident company) /= (-1) then do
            let newCompany = company { name = name }
            editCompanyJSON "./Data/Companies.json" newCompany
            return True
        else do
            putStrLn "\nOcorreu um problema! A Empresa com este id não foi encontrada!"
            return False
    else do
        putStrLn "\nOcorreu um problema! O nome da empresa deve ter no máximo 18 caracteres."
        return False

setAge :: Int -> Int -> IO Bool
setAge id age = do
    let company = getCompany id
    if (ident company) /= 0 then do
        let newCompany = company { age = age }
        editCompanyJSON "./Data/Companies.json" newCompany
        return True
    else do
        putStrLn "\nOcorreu um problema! A Empresa com este id não foi encontrada!"
        return False

setCNPJ :: Int -> String -> IO Bool
setCNPJ id cnpj = do
    let company = getCompany id
    if (length (show cnpj)) == 14 then do
        if (ident company) /= (-1) then do
            let newCompany = company { cnpj = cnpj }
            editCompanyJSON "./Data/Companies.json" newCompany
            return True
        else do 
            putStrLn "\nOcorreu um problema! A Empresa com este id não foi encontrada!"
            return False
    else do 
        putStrLn "\nOcorreu um problema! O CNPJ não contém 14 dígitos."
        return False

setActuation :: Int -> String -> IO Bool
setActuation id actuation = do
    let company = getCompany id
    if (length actuation) <= 86 then do
        if (ident company) /= (-1) then do
            let newCompany = company { actuation = actuation }
            editCompanyJSON "./Data/Companies.json" newCompany
            return True
        else do
            putStrLn "\nOcorreu um problema! A Empresa com este id não foi encontrada!"
            return False
    else do
        putStrLn "\nOcorreu um problema! A área de atuação da Empresa deve ter no máximo 86 caracteres!"
        return False

setDeclaration :: Int -> String -> IO Bool
setDeclaration id declaration = do
    let company = getCompany id
    if (length declaration) <= 15 then do
        if (ident company) /= (-1) then do
            let newCompany = company { declaration = declaration }
            editCompanyJSON "./Data/Companies.json" newCompany
            return True
        else do
            putStrLn "\nOcorreu um problema! A Empresa com este id não foi encontrada!"
            return False
    else do
        putStrLn "\nOcorreu um problema! A declaração da Empresa deve ter no máximo 15 caracteres!"
        return False

setCode :: Int -> String -> IO Bool
setCode id code = do
    let company = getCompany id
    if (length code) <= 15 then do
        if (ident company) /= (-1) then do
            let newCompany = company { code = (uppercaseString code) }
            editCompanyJSON "./Data/Companies.json" newCompany
            return True
        else do
            putStrLn "\nOcorreu um problema! A Empresa com este id não foi encontrada!"
            return False
    else do
        putStrLn "\nOcorreu um problema! O código da Empresa deve ter 5 caracteres!"
        return False

setPrice :: Int -> Float -> IO Bool
setPrice id price = do 
    let company = getCompany id
    if (ident company) /= (-1) then do
        let newCompany = company { price = price }
        editCompanyJSON "./Data/Companies.json" newCompany
        return True
    else do
        putStrLn "\nOcorreu um problema! A Empresa com este id não foi encontrada!"
        return False

setTrendIndicator:: Int -> String -> IO ()
setTrendIndicator id trendIndicator = do
    let company = getCompany id
    if (ident company) /= (-1) then do
        let newCompany = company { trendIndicator = trendIndicator }
        editCompanyJSON "./Data/Companies.json" newCompany
    else
        putStrLn "\nOcorreu um problema! A Empresa com este id não foi encontrada!"

setMinPrice :: Int -> Float -> IO Bool
setMinPrice id minPrice = do 
    let company = getCompany id
    if (ident company) /= (-1) then do
        let newCompany = company { minPrice = minPrice }
        editCompanyJSON "./Data/Companies.json" newCompany
        return True
    else do
        putStrLn "\nOcorreu um problema! A Empresa com este id não foi encontrada!"
        return False

setMaxPrice :: Int -> Float -> IO Bool
setMaxPrice id maxPrice = do 
    let company = getCompany id
    if (ident company) /= (-1) then do
        let newCompany = company { maxPrice = maxPrice }
        editCompanyJSON "./Data/Companies.json" newCompany
        return True
    else do
        putStrLn "\nOcorreu um problema! A Empresa com este id não foi encontrada!"
        return False

setStartPrice :: Int -> Float -> IO Bool
setStartPrice id startPrice = do 
    let company = getCompany id
    if (ident company) /= (-1) then do
        let newCompany = company { startPrice = startPrice }
        editCompanyJSON "./Data/Companies.json" newCompany
        return True
    else do
        putStrLn "\nOcorreu um problema! A Empresa com este id não foi encontrada!"
        return False

setRow :: Int -> Int -> IO Bool
setRow id row = do
    let company = getCompany id
    if (ident company) /= (-1) then do
        let newCompany = company { row = row }
        editCompanyJSON "./Data/Companies.json" newCompany
        return True
    else do
        putStrLn "\nOcorreu um problema! A Empresa com este id não foi encontrada!"
        return False

setCol :: Int -> Int -> IO Bool
setCol id col = do
    let company = getCompany id
    if (ident company) /= (-1) then do
        let newCompany = company { col = col }
        editCompanyJSON "./Data/Companies.json" newCompany
        return True
    else do
        putStrLn "\nOcorreu um problema! A Empresa com este id não foi encontrada!"
        return False

updateRow :: Int -> Int -> IO()
updateRow id addRow = do
    let company = getCompaniesByID id (getCompanyJSON "./Data/Companies.json")
    let newCompany = company {row = getRow id + addRow}
    editCompanyJSON "./Data/Companies.json" newCompany

updateCol :: Int -> Int -> IO()
updateCol id addCol = do
    let company = getCompaniesByID id (getCompanyJSON "./Data/Companies.json")
    let newCompany = company {col = (getCol id) + addCol}
    editCompanyJSON "./Data/Companies.json" newCompany

-- addPrice :: Int -> Float -> IO()
-- addPrice id acaoAdicional = do
--     let company = getCompaniesByID id (getCompanyJSON "./Data/Companies.json")
--     let newPrice = fromIntegral (round ((price company + acaoAdicional) * 10 )) / 10    -- formata o valor para somente uma casa decimal
--     let newCompany = company {price = newPrice}
--     editCompanyJSON "./Data/Companies.json" newCompany

formatCNPJ :: String -> String
formatCNPJ cnpj =
  let cnpjStr = cnpj
  in if length cnpjStr == 14
       then
         let (part1, rest1) = splitAt 2 cnpjStr
             (part2, rest2) = splitAt 3 rest1
             (part3, rest3) = splitAt 3 rest2
             (part4, part5) = splitAt 4 rest3
         in part1 ++ "." ++ part2 ++ "." ++ part3 ++ "/" ++ part4 ++ "-" ++ part5
       else "CNPJ inválido"

uppercaseString :: String -> String
uppercaseString = map toUpper
