module Client.SearchCompany where
import Client.ModelCompany
import Client.SaveCompany
import Data.List (isInfixOf)

-- Filtro por nome
getCompaniesWithNameSubstring :: String -> [Company] -> [Company]
getCompaniesWithNameSubstring substring companies = filter (\company -> substring `isInfixOf` name company) companies

searchAndFilterCompanyByName :: String -> [Company]
searchAndFilterCompanyByName substring = getCompaniesWithNameSubstring substring (getCompanyJSON "../Data/Companies.json")

-- Filtro por Atuação
getCompaniesWithActingSubstring :: String -> [Company] -> [Company]
getCompaniesWithActingSubstring substring companies = filter (\company -> substring `isInfixOf` actuation company) companies

searchAndFilterCompanyByActing :: String -> [Company]
searchAndFilterCompanyByActing substring = getCompaniesWithActingSubstring substring (getCompanyJSON "../Data/Companies.json")

-- Filtro por código
getCompaniesWithPasswordCode :: Int -> [Company] -> [Company]
getCompaniesWithPasswordCode code_ companies = filter (\company -> code_ == (code company)) companies

searchAndFilterCompanyByCode :: Int -> [Company]
searchAndFilterCompanyByCode code = getCompaniesWithPasswordCode code (getCompanyJSON "../Data/Companies.json")

-- Filtro por nome e atuação simultaneamente
searchAndFilterBySome :: String -> [Company]
searchAndFilterBySome substring = (searchAndFilterCompanyByName substring) ++ (searchAndFilterCompanyByActing substring)
