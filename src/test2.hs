import Company.SaveCompany (getCompanyJSON, identifySequenceBreak)
import Company.ModelCompany (Company (ident))
import Data.List (sort)




identifyIdSequenceBreak :: [Int] -> Int
identifyIdSequenceBreak [] = 1
identifyIdSequenceBreak (x:xs)
    | not (null xs) && x + 1 /= head xs = x + 1
    | null xs = x + 1
    | otherwise = identifyIdSequenceBreak xs


getIds :: [Company] -> [Int]
getIds companies = sort [ident x | x <- companies]


main :: IO ()
main = do
    print (identifyIdSequenceBreak (getIds (getCompanyJSON "./Data/Companies.json")))