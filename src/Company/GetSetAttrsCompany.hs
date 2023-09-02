module Company.GetSetAttrsCompany where
import Company.SaveCompany
import Company.ModelCompany

getSaldo :: Int -> Float
getSaldo id = (actions (getCompaniesByID id (getCompanyJSON "./Data/Companies.json")))

getCNPJ :: Int -> String
getCNPJ id = formatCNPJ (cnpj (getCompaniesByID id (getCompanyJSON "./Data/Companies.json")))

getNome :: Int -> String
getNome id = (name (getCompaniesByID id (getCompanyJSON "./Data/Companies.json")))

setSaldo :: Int -> Float -> IO()
setSaldo id acaoAdicional = do
    let company = getCompaniesByID id (getCompanyJSON "./Data/Companies.json")
    let newActions = fromIntegral (round (((actions company) + acaoAdicional) * 10 )) / 10    -- formata o valor para somente uma casa decimal
    let newCompany = company {actions = newActions}
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