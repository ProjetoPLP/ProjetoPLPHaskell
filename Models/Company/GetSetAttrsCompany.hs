module Models.Company.GetSetAttrsCompany where

import Data.Char ( toUpper )
import Models.Company.SaveCompany ( editCompanyJSON, getCompaniesByID, getCompany, getCompanyJSON )
import Models.Company.ModelCompany ( Company(col, name, age, cnpj, actuation, declaration, code, price, trendIndicator, minPrice, maxPrice, startPrice, ident, row) )


getName :: Int -> String
getName id = name (getCompany id)


getAge :: Int -> String
getAge id = age (getCompany id)


getCNPJ :: Int -> String
getCNPJ id = cnpj (getCompany id)


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
getIdent = ident


setPrice :: Int -> Float -> IO ()
setPrice id price = do
    let company = getCompany id
    if ident company /= (-1) then do
        let newCompany = company { price = price }
        editCompanyJSON "./Data/Companies.json" newCompany
    else do
        putStrLn "\nOcorreu um problema! A Empresa com este id não foi encontrada!"


setTrendIndicator:: Int -> String -> IO ()
setTrendIndicator id trendIndicator = do
    let company = getCompany id
    if (ident company) /= (-1) then do
        let newCompany = company { trendIndicator = trendIndicator }
        editCompanyJSON "./Data/Companies.json" newCompany
    else
        putStrLn "\nOcorreu um problema! A Empresa com este id não foi encontrada!"


setMinPrice :: Int -> Float -> IO ()
setMinPrice id minPrice = do
    let company = getCompany id
    if (ident company) /= (-1) then do
        let newCompany = company { minPrice = minPrice }
        editCompanyJSON "./Data/Companies.json" newCompany
    else do
        putStrLn "\nOcorreu um problema! A Empresa com este id não foi encontrada!"


setMaxPrice :: Int -> Float -> IO ()
setMaxPrice id maxPrice = do
    let company = getCompany id
    if (ident company) /= (-1) then do
        let newCompany = company { maxPrice = maxPrice }
        editCompanyJSON "./Data/Companies.json" newCompany
    else do
        putStrLn "\nOcorreu um problema! A Empresa com este id não foi encontrada!"


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