module GetSetAttrsCompany where
import SaveCompany
import ModelCompany

getSaldo :: Int -> Float
getSaldo id = (actions (getCompaniesByID id (getCompanyJSON "../Data/Companies.json")))

getCNPJ :: Int -> Int
getCNPJ id = (cnpj (getCompaniesByID id (getCompanyJSON "../Data/Companies.json")))

getNome :: Int -> String
getNome id = (name (getCompaniesByID id (getCompanyJSON "../Data/Companies.json")))

setSaldo :: Int -> Float -> IO()
setSaldo id acaoAdicional = do
    let company = getCompaniesByID id (getCompanyJSON "../Data/Companies.json")
    let newActions = (actions company) + acaoAdicional
    let newCompany = company {actions = newActions}
    editCompanyJSON "../Data/Companies.json" newCompany
