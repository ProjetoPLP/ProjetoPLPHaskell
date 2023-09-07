module Company.GetSetAttrsCompany where
import Company.SaveCompany
import Company.ModelCompany

getName :: Int -> Maybe String
getName id = do
    let companyName = name (getCompany id)
    if companyName == ""
    then Nothing
    else Just companyName

getAge :: Int -> Maybe Int
getAge id = do
    let companyAge = age (getCompany id)
    if companyAge == 0
    then Nothing
    else Just companyAge

getCNPJ :: Int -> Maybe String
getCNPJ id = do
    let companyCNPJ = cnpj (getCompany id)
    if length (show(companyCNPJ)) /= 14 
        then Nothing
        else Just (formatCNPJ(companyCNPJ))

getActuation :: Int -> Maybe String
getActuation id = do
    let companyActuation = actuation (getCompany id)
    if companyActuation == ""
        then Nothing
        else Just companyActuation

getDeclaration :: Int -> Maybe String
getDeclaration id = do
    let companyDeclaration = declaration (getCompany id)
    if companyDeclaration == ""
        then Nothing
        else Just companyDeclaration

getCode :: Int -> Maybe String
getCode id = do
    let companyCode = code (getCompany id)
    if  companyCode == ""
        then Nothing
        else Just companyCode

getPrice :: Int -> Float
getPrice id = price (getCompany id)

getRow :: Int -> Int
getRow id = row (getCompany id)

getCol :: Int -> Int
getCol id = col (getCompany id)

-- ==========================

setName :: Int -> String -> IO Bool
setName id name = do
    let company = getCompany id
    if (ident company) /= (-1) then do
        let newCompany = company { name = name }
        editCompanyJSON "./Data/Companies.json" newCompany
        return True
    else do
        putStrLn "\nOcorreu um problema! A Empresa com este id não foi encontrada!"
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

setCNPJ :: Int -> Int -> IO Bool
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
    if (ident company) /= (-1) then do
        let newCompany = company { actuation = actuation }
        editCompanyJSON "./Data/Companies.json" newCompany
        return True
    else do
        putStrLn "\nOcorreu um problema! A Empresa com este id não foi encontrada!"
        return False

setDeclaration :: Int -> String -> IO Bool
setDeclaration id declaration = do
    let company = getCompany id
    if (ident company) /= (-1) then do
        let newCompany = company { declaration = declaration }
        editCompanyJSON "./Data/Companies.json" newCompany
        return True
    else do
        putStrLn "\nOcorreu um problema! A Empresa com este id não foi encontrada!"
        return False

setCode :: Int -> String -> IO Bool
setCode id code = do
    let company = getCompany id
    if (ident company) /= (-1) then do
        let newCompany = company { code = code }
        editCompanyJSON "./Data/Companies.json" newCompany
        return True
    else do
        putStrLn "\nOcorreu um problema! A Empresa com este id não foi encontrada!"
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


-- ======================================================
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

addPrice :: Int -> Float -> IO()
addPrice id acaoAdicional = do
    let company = getCompaniesByID id (getCompanyJSON "./Data/Companies.json")
    let newPrice = fromIntegral (round ((price company + acaoAdicional) * 10 )) / 10    -- formata o valor para somente uma casa decimal
    let newCompany = company {price = newPrice}
    editCompanyJSON "./Data/Companies.json" newCompany

formatCNPJ :: Int -> String
formatCNPJ cnpj =
  let cnpjStr = show cnpj
  in if length cnpjStr == 14
       then
         let (part1, rest1) = splitAt 2 cnpjStr
             (part2, rest2) = splitAt 3 rest1
             (part3, rest3) = splitAt 3 rest2
             (part4, part5) = splitAt 4 rest3
         in part1 ++ "." ++ part2 ++ "." ++ part3 ++ "/" ++ part4 ++ "-" ++ part5
       else "CNPJ inválido"
