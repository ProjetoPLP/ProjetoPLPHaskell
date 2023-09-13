import Company.SaveCompany (getCompanyJSON, identifySequenceBreak)



main :: IO ()
main = do
    print (identifySequenceBreak (getCompanyJSON "./Data/Companies.json"))
