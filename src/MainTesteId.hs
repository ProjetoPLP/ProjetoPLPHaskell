import Company.ModelCompany
import Company.SaveCompany

identifySequenceBreak :: [Company] -> Int
identifySequenceBreak companies
  | null companies = 1
  | otherwise = go 1 companies
  where
    go _ [] = length companies + 1
    go n (Company i _ _ _ _ _ _ _ _ _ _ _ _ _ : rest)
      | n == i = go (n + 1) rest
      | otherwise = n

main :: IO()
main = do
    let comp1 = Company 1 "" 0 "" "" "" "" 0.00 "" 0.00 0.00 0.00 0 0
    let comp2 = Company 2 "" 0 "" "" "" "" 0.00 "" 0.00 0.00 0.00 0 0
    let comp3 = Company 3 "" 0 "" "" "" "" 0.00 "" 0.00 0.00 0.00 0 0
    let comp4 = Company 4 "" 0 "" "" "" "" 0.00 "" 0.00 0.00 0.00 0 0
    let comp5 = Company 5 "" 0 "" "" "" "" 0.00 "" 0.00 0.00 0.00 0 0
    let comp6 = Company 6 "" 0 "" "" "" "" 0.00 "" 0.00 0.00 0.00 0 0

    let allComp = [comp1, comp2, comp3, comp4, comp6]
    removeCompanyJSON "./Data/Companies.json" 3
