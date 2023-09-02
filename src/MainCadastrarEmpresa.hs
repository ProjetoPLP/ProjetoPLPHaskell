import Company.CadastrarCompany
import Company.SearchCompany

main :: IO()
main = do
    result <- cadastrarCompany
    putStrLn (result)
